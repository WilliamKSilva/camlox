type state = {
  start : int; (* Points to the first character in the lexeme being scanned *)
  current : int;
      (* Points at the current character being considered in the lexeme being scanned *)
  line : int; (* Tracks what source line "current" is on*)
}

let is_at_end current source = current >= String.length source
let update_state_start state start = { state with start }

let advance state source =
  let state = { state with current = state.current + 1 } in
  (state, source.[state.current - 1])

let rec scan_tokens state source tokens =
  let add_token state literal token_type =
    let text = String.sub source state.start state.current in
    let token : Token.token =
      { token_type; line = state.line; literal; lexeme = text }
    in
    List.append tokens [ token ]
  in
  let scan_token state =
    let state, c = advance state source in
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
    | _ -> (state, tokens)
  in

  if is_at_end state.current source then
    let token : Token.token =
      { token_type = Token.EOF; lexeme = ""; literal = None; line = state.line }
    in
    List.append tokens [ token ]
  else
    let state = update_state_start state state.current in
    let state, tokens = scan_token state in
    scan_tokens state source tokens
