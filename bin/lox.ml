let run source =
  print_endline source

let run_prompt =
  print_endline "> ";

  let rec loop () =
    let line = read_line () in
    run line;
    loop ()
  in

  try 
    loop () 
  with End_of_file -> ()

let run_file path =
  let chan = open_in path in
  let read_chan =
    let buf = Buffer.create 4096 in
    let rec loop () =
      let line = input_line chan in
      Buffer.add_string buf line;
      loop ()
    in
    try 
      loop ()
    with End_of_file -> Buffer.contents buf
  in
  run read_chan

let () = 
  let files = Sys.argv in
  if Array.length files == 1 then 
    run_file (Array.get files 1)
  else
    run_prompt