let rec gcd a b = if b == 0 then a else gcd b (a mod b)

let gen_simai (l_tm, l_ho) arr_map ochan = if List.is_empty l_tm || List.is_empty l_ho then () else
  let first = Types.start_ho (List.hd l_ho) in
  Printf.fprintf ochan "&first=%f\n" ((float_of_int first) /. 1000.);
  let bpm = Types.bpm_tm (List.hd l_tm) in
  Printf.fprintf ochan "(%.3f)" bpm;
  let state_next (time, bpm, pat_div, l_tm, l_ho) = if List.is_empty l_tm && List.is_empty l_ho then None else
  (
    let rec get_tms time l_tm = match l_tm with
    [] -> ([], [])
    | tm::l -> if Types.start_tm tm == time then
      let l2 = get_tms time l in (tm::(fst l2), snd l2)
    else ([], l_tm) in

    let rec get_hos time l_ho = match l_ho with
    [] -> ([], [])
    | ho::l -> if Types.start_ho ho == time then
      let l2 = get_hos time l in (ho::(fst l2), snd l2)
    else ([], l_ho) in

    let rec get_next_bpm tms = match tms with
    [] -> None
    | tm::l -> match tm with
      Types.Red { bpm = b; _ } -> Some b
      | Types.Green _ -> get_next_bpm l
    in

    let (tms, l_tm_next) = get_tms time l_tm in
    let (hos, l_ho_next) = get_hos time l_ho in
    let bpm_next = match get_next_bpm tms with None -> bpm | Some b -> b in

    if bpm_next != bpm then Printf.fprintf ochan "\n(%.3f)" bpm_next else ();

    let time_next = match (l_tm_next, l_ho_next) with
      ([], []) -> Int.max_int
      | ([], ho::_) -> Types.start_ho ho
      | (tm::_, []) -> Types.start_tm tm
      | (tm::_, ho::_) -> Int.max (Types.start_tm tm) (Types.start_ho ho)
    in

    let (pat_n, pat_div_next) = if time_next == Int.max_int then (0, pat_div) else
      let pat = int_of_float ((float_of_int (time_next - time)) *. 96. /. 60000. *. bpm_next +. 0.5) in
      let g = gcd pat 384 in
      let (n, d) = (pat / g, 384 / g) in
      if pat_div == 0 || pat_div mod d != 0 || (pat_div mod d == 0 && pat_div / d >= 4) then
        (Printf.fprintf ochan "\n{%d}" d; (n, d))
      else
        let m = pat_div / d in (n * m, pat_div)
    in

    let rec print_hos hos = match hos with
    [] -> ()
    | ho::l -> match ho with
      Types.Tap { pos = p; _ } -> Printf.fprintf ochan "/%d" arr_map.(p)
      | Types.Long { pos = p; start = s; end_t = e} -> 
        let pat = int_of_float ((float_of_int (e - s)) *. 96. /. 60000. *. bpm_next +. 0.5) in
        let g = gcd pat 384 in
        let (n, d) = (pat / g, 384 / g) in
        Printf.fprintf ochan "/%dh[%d:%d]" arr_map.(p) n d;
      print_hos l
    in

    if not (List.is_empty hos) then
      (match List.hd hos with
      Types.Tap { pos = p; _ } -> Printf.fprintf ochan "%d" arr_map.(p)
      | Types.Long { pos = p; start = s; end_t = e} -> 
        let pat = int_of_float ((float_of_int (e - s)) *. 96. /. 60000. *. bpm_next +. 0.5) in
        let g = gcd pat 384 in
        let (n, d) = (pat / g, 384 / g) in
        Printf.fprintf ochan "%dh[%d:%d]" arr_map.(p) n d;
      print_hos (List.tl hos));

    let rec print_comma n = if n <= 0 then () else
      (Printf.fprintf ochan ","; print_comma (n-1)) in print_comma pat_n;
    
    Some (time_next, bpm_next, pat_div_next, l_tm_next, l_ho_next)
  )
  in

  let rec loop st_opt = match st_opt with None -> () | Some st -> loop (state_next st) in

  loop (Some (first, bpm, 0, List.tl l_tm, l_ho)); flush ochan