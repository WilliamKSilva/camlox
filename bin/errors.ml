let error line where message =
  let line_to_str = string_of_int line in
  "[line ]" ^ line_to_str ^ "] Error" ^ where ^ ": " ^ message
