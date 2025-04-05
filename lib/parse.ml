let _tag_tm = "[TimingPoints]"
let _tag_ho = "[HitObjects]"

let input_line_opt ichan = 
  try Some (input_line ichan, ichan)
  with End_of_file -> None

type input_state = (bool * bool * (string * in_channel) option)

let input_state_next (s : input_state) : input_state = match s with
  (_, _, None) -> (false, false, None)
  | (is_tm, is_ho, Some(s, ichan)) -> 
    match input_line_opt ichan with
    None -> (false, false, None)
    | Some (s_new, ichan_new) -> 
      if String.equal s _tag_tm then (true, false, Some (s_new, ichan_new))
      else if String.equal s _tag_ho then (false, true, Some (s_new, ichan_new))
      else if (String.length s_new) >= 1 && s_new.[0] == '[' then
        (false, false, Some (s_new, ichan_new))
      else (is_tm, is_ho, Some (s_new, ichan_new))

let parse_ho s : Types.hit_obj =
  let args = Array.of_list (List.map int_of_string_opt ((String.split_on_char ':' (String.trim s)) |> List.hd |> String.split_on_char ',')) in
  let x = Option.get args.(0) in
  let start = Option.get args.(2) in
  if (Int.logand (Option.get args.(3)) 1) == 1 then 
    Types.Tap { pos = x * 4 / 512; start = start }
  else
    let end_t = Option.get args.(5) in
    Types.Long { pos = x * 4 / 512; start = start; end_t = end_t }

let parse_tm s : Types.timing =
  let args = Array.of_list (List.map float_of_string_opt (String.split_on_char ',' (String.trim s))) in
  let start = int_of_float (Option.get args.(0)) in
  let a1 = Option.get args.(1) in
  if (Option.get args.(6)) == 0. then
    Green { start = start; speed = 100. /. a1 }
  else
    let pat = int_of_float (Option.get args.(3)) in
    Red { start = start; bpm = 60000. /. a1; pat = pat }

let parse_osu ichan =
  let comb_tm tm l2 = (tm::(fst l2), snd l2) in
  let comb_ho ho l2 = (fst l2, ho::(snd l2)) in
  let rec inner (st : input_state) = 
    match st with
    (_, _, None) -> ([], [])
    | (true, _, Some (s, _)) -> if (String.length s) == 0 then inner (input_state_next st) else
      let tm = parse_tm s in
      comb_tm tm (inner (input_state_next st))
    | (_, true, Some (s, _)) -> if (String.length s) == 0 then inner (input_state_next st) else
      let ho = parse_ho s in
      comb_ho ho (inner (input_state_next st))
    | (false, false, Some _) -> inner (input_state_next st)
    in inner (false, false, Some ("", ichan))
      
let print_tm (tm : Types.timing) = match tm with
  Red { start = s; bpm = b; pat = p } -> Printf.printf "Red: {%d, %f, %d}\n" s b p
  | Green { start = s; speed = sp } -> Printf.printf "Green: {%d, %f}\n" s sp

let print_ho (ho : Types.hit_obj) = match ho with
  Tap { start = s; pos = p } -> Printf.printf "Tap: {%d, %d}\n" p s
  | Long { start = s; end_t = e; pos = p } -> Printf.printf "Long: {%d, %d, %d}\n" s e p
