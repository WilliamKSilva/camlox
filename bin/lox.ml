module Token = Camlox.Token
module Scanner = Camlox.Scanner

let run source =
  let identifiers = Token.get_identifiers in
  let state : Scanner.state = { current = 0; start = 0; line = 1 } in
  let tokens = Scanner.scan_tokens state source [] identifiers in
  match tokens with
  | [] -> print_endline "nada"
  | hd :: _ -> print_endline hd.lexeme

let run_prompt =
  print_endline "> ";

  let rec loop () =
    let line = read_line () in
    run line;
    loop ()
  in

  try loop () with End_of_file -> ()

let run_file path =
  let chan = open_in path in
  let read_chan =
    let buf = Buffer.create 4096 in
    let rec loop () =
      let line = input_line chan in
      Buffer.add_string buf line;
      loop ()
    in
    try loop () with End_of_file -> Buffer.contents buf
  in
  let has_error = false in
  if has_error then raise Exit else run read_chan

let () =
  let files = Sys.argv in
  if Array.length files == 1 then
    try run_file (Array.get files 1) with Exit -> exit 65
  else run_prompt
