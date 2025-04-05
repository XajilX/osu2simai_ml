module Parse = Osu2mai.Parse
module Gen = Osu2mai.Gen_simai

let () = 
  let len_argv = Array.length Sys.argv in
  if len_argv < 2 then
    print_string "arguments insufficient"
  else
    let filename = Sys.argv.(len_argv - 1) in
    let ichan = open_in filename in
    let l2 = Parse.parse_osu ichan in
    print_endline "parse complete";
    let arr_map = [|6;5;4;3|] in
    let filename_out = "majdata.txt" in
    let ochan = open_out filename_out in
    Gen.gen_simai l2 arr_map ochan;
    close_out ochan
