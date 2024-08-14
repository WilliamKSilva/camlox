exception Syntax_error

let report line where message =
  let error = "[line ]" ^ line ^ "] Error" ^ where ^ ": " ^ message in
  print_endline error;
  raise (Syntax_error)

let error line message =
  report line "" message