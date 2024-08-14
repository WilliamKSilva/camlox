type state = {
  start : int; (* Points to the first character in the lexeme being scanned *)
  current : int;
      (* Points at the current character in the lexeme being scanned *)
  line : int; (* Tracks what source line "current" is on*)
}

let update_state_start state start = { state with start }
let update_state_current state current = { state with current }

let rec scan_tokens state source tokens =
  let is_at_end current = current >= String.length source in
  let advance state =
    (*
       "advance" consumes the current char, so we return its value and points
       our state to the next char that will be consumed in the next iteration
    *)
    let state = { state with current = state.current + 1 } in
    (state, source.[state.current - 1])
  in
  let add_token state literal token_type =
    let text = String.sub source state.start state.current in
    let token : Token.token =
      { token_type; line = state.line; literal; lexeme = text }
    in
    List.append tokens [ token ]
  in
  let scan_token state =
    let match_char expected =
      (*
        "match_char" checks if the current char it is what we are expecting.
        If it is we consume it, it is used to check if we have a double character
        lexeme, like != or >=
      *)
      if is_at_end state.current then (state, false)
      else if String.get source state.current != expected then (state, false)
      else
        let state = update_state_current state (state.current + 1) in
        (state, true)
    in

    let peek state =
      if is_at_end state.current then Char.chr 0 (* \0 *)
      else String.get source state.current
    in

    let state, c = advance state in
    match c with
    | '(' -> (state, add_token state None Token.LEFT_PAREN)
    | ')' -> (state, add_token state None Token.RIGHT_PAREN)
    | '{' -> (state, add_token state None Token.LEFT_BRACE)
    | '}' -> (state, add_token state None Token.RIGHT_BRACE)
    | ',' -> (state, add_token state None Token.COMMA)
    | '.' -> (state, add_token state None Token.DOT)
    | '-' -> (state, add_token state None Token.MINUS)
    | '+' -> (state, add_token state None Token.PLUS)
    | ';' -> (state, add_token state None Token.SEMICOLON)
    | '*' -> (state, add_token state None Token.STAR)
    | '!' ->
        let state, matched = match_char '=' in
        let tokens =
          if matched then add_token state None Token.BANG_EQUAL
          else add_token state None Token.BANG
        in
        (state, tokens)
    | '=' ->
        let state, matched = match_char '=' in
        let tokens =
          if matched then add_token state None Token.EQUAL_EQUAL
          else add_token state None Token.EQUAL
        in
        (state, tokens)
    | '<' ->
        let state, matched = match_char '=' in
        let tokens =
          if matched then add_token state None Token.LESS_EQUAL
          else add_token state None Token.LESS
        in
        (state, tokens)
    | '>' ->
        let state, matched = match_char '=' in
        let tokens =
          if matched then add_token state None Token.GREATER_EQUAL
          else add_token state None Token.GREATER
        in
        (state, tokens)
    | '/' ->
        (* This loop will consume characters until end of the line is found *)
        let rec loop state =
          if is_at_end state.current == false && peek state != '\n' then
            let state, _ = advance state in
            loop state
          else state
        in

        let state, matched = match_char '/' in
        if matched then
          let state = loop state in
          (state, tokens)
        else
          let tokens = add_token state None Token.SLASH in
          (state, tokens)
    | ' ' -> (state, tokens)
    | '\r' -> (state, tokens)
    | '\t' -> (state, tokens)
    | '\n' ->
        let state = { state with line = state.line + 1 } in
        (state, tokens)
    | _ ->
        Errors.error state.line "Unexpected Char.";
        (state, tokens)
  in

  if is_at_end state.current then
    let token : Token.token =
      { token_type = Token.EOF; lexeme = ""; literal = None; line = state.line }
    in
    List.append tokens [ token ]
  else
    let state = update_state_start state state.current in
    try
      let state, tokens = scan_token state in
      scan_tokens state source tokens
    with Errors.Syntax_error ->
      (*
         If a syntax error is encoutered we keep scanning, but in the future we
         may want to implement additional stuff. So we still track the Exception 
         when calling scan_token
      *)
      let state, tokens = scan_token state in
      scan_tokens state source tokens
