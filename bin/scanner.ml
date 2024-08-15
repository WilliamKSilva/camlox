type state = {
  start : int; (* Points to the first character in the lexeme being scanned *)
  current : int;
      (* Points at the current character in the lexeme being scanned *)
  line : int; (* Tracks what source line "current" is on*)
}

type scanner_data = state * Token.token list
type scanner_error = { message : string; data : scanner_data }
type scanner_response = (scanner_data, scanner_error) result

let update_state_start state start = { state with start }
let update_state_current state current = { state with current }

let rec scan_tokens state source tokens =
  let is_at_end current = current >= String.length source in
  let scan_token state =
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
      (* Peek look at the current char without consuming it *)
      if is_at_end state.current then Char.chr 0 (* \0 *)
      else String.get source state.current
    in

    let state, c = advance state in
    match c with
    | '(' -> Ok (state, add_token state None Token.LEFT_PAREN)
    | ')' -> Ok (state, add_token state None Token.RIGHT_PAREN)
    | '{' -> Ok (state, add_token state None Token.LEFT_BRACE)
    | '}' -> Ok (state, add_token state None Token.RIGHT_BRACE)
    | ',' -> Ok (state, add_token state None Token.COMMA)
    | '.' -> Ok (state, add_token state None Token.DOT)
    | '-' -> Ok (state, add_token state None Token.MINUS)
    | '+' -> Ok (state, add_token state None Token.PLUS)
    | ';' -> Ok (state, add_token state None Token.SEMICOLON)
    | '*' -> Ok (state, add_token state None Token.STAR)
    | '!' ->
        let state, matched = match_char '=' in
        let tokens =
          if matched then add_token state None Token.BANG_EQUAL
          else add_token state None Token.BANG
        in
        Ok (state, tokens)
    | '=' ->
        let state, matched = match_char '=' in
        let tokens =
          if matched then add_token state None Token.EQUAL_EQUAL
          else add_token state None Token.EQUAL
        in
        Ok (state, tokens)
    | '<' ->
        let state, matched = match_char '=' in
        let tokens =
          if matched then add_token state None Token.LESS_EQUAL
          else add_token state None Token.LESS
        in
        Ok (state, tokens)
    | '>' ->
        let state, matched = match_char '=' in
        let tokens =
          if matched then add_token state None Token.GREATER_EQUAL
          else add_token state None Token.GREATER
        in
        Ok (state, tokens)
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
          Ok (state, tokens)
        else
          let tokens = add_token state None Token.SLASH in
          Ok (state, tokens)
    | ' ' -> Ok (state, tokens)
    | '\r' -> Ok (state, tokens)
    | '\t' -> Ok (state, tokens)
    | '\n' ->
        let state = { state with line = state.line + 1 } in
        Ok (state, tokens)
    | '"' ->
        let rec loop_string state =
          if peek state != '"' && is_at_end state.current == false then
            let state =
              if peek state == '\n' then { state with line = state.line + 1 }
              else state
            in
            let state, _ = advance state in
            loop_string state
          else state
        in

        let state = loop_string state in
        (*
           If the scanning is at the end of the source here,
           we have not found the end '"' char.
        *)
        if is_at_end state.current then
          Error
            {
              message = Errors.error state.line "" "Undetermined string.";
              data = (state, tokens);
            }
        else
          let state, _ = advance state in
          (*
             The second argument of String.sub is the length of the substring,
             so current - 1 would be the '"' string closing,
             and current - 2 it is the actual size of the string starting from the
             index at start + 1 after the opening '"'
          *)
          let value = String.sub source (state.start + 1) (state.current - 2) in
          let tokens = add_token state (StrLiteral value) STRING in
          Ok (state, tokens)
    | _ ->
        Error
          {
            message = Errors.error state.line "" "Unexpected char.";
            data = (state, tokens);
          }
  in

  if is_at_end state.current then
    let token : Token.token =
      { token_type = Token.EOF; lexeme = ""; literal = None; line = state.line }
    in
    List.append tokens [ token ]
  else
    let state = update_state_start state state.current in
    let res = scan_token state in
    match res with
    | Ok (_, tokens) -> tokens
    | Error e ->
        print_endline e.message;
        let _, tokens = e.data in
        tokens
