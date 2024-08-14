exception Syntax_error

let report line where message =
  let line_to_str = string_of_int line in
  let error = "[line ]" ^ line_to_str ^ "] Error" ^ where ^ ": " ^ message in
  print_endline error;
  let _ = raise Syntax_error in
  ()

let error line message = report line "" message
