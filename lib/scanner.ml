open Token
open Errors

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

let rec scan_tokens state source tokens (identifiers : Token.identifiers) =
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
    let match_char state expected =
      (*
        "match_char" checks if the current char it is what we are expecting.
        If it is we consume it, it is used to check if we have a double character
        lexeme, like != or >=
      *)
      if is_at_end state.current then (state, false)
      else if String.get source state.current != expected then (state, false)
      else
        let state, _ = advance state in
        (state, true)
    in
    let peek state =
      (* Peek look at the current char without consuming it *)
      if is_at_end state.current then '\000' (* \0 *)
      else String.get source state.current
    in

    let peek_next state =
      if state.current + 1 >= String.length source then (* \0 *)
        Char.chr 0
      else source.[state.current + 1]
    in

    let add_token state literal token_type =
      (* TODO: check if this substring is right *)
      let text = String.sub source state.start state.current in

      let token : Token.token =
        { token_type; line = state.line; literal; lexeme = text }
      in
      List.append tokens [ token ]
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
        let state, matched = match_char state '=' in
        if matched then
          let tokens = add_token state None Token.BANG_EQUAL in
          Ok (state, tokens)
        else
          let tokens = add_token state None Token.BANG in
          Ok (state, tokens)
    | '=' ->
        let state, matched = match_char state '=' in
        if matched then
          let tokens = add_token state None Token.EQUAL_EQUAL in
          Ok (state, tokens)
        else
          let tokens = add_token state None Token.EQUAL in
          let state, _ = advance state in
          Ok (state, tokens)
    | '<' ->
        let state, matched = match_char state '=' in
        if matched then
          let tokens = add_token state None Token.LESS_EQUAL in
          Ok (state, tokens)
        else
          let tokens = add_token state None Token.LESS in
          let state, _ = advance state in
          Ok (state, tokens)
    | '>' ->
        let state, matched = match_char state '=' in
        if matched then
          let tokens = add_token state None Token.GREATER_EQUAL in
          Ok (state, tokens)
        else
          let tokens = add_token state None Token.GREATER in
          let state, _ = advance state in
          Ok (state, tokens)
    | '/' ->
        (* This loop will consume characters until end of the line is found *)
        let rec loop state =
          if is_at_end state.current == false && peek state != '\n' then
            let state, _ = advance state in
            loop state
          else state
        in

        let state, matched = match_char state '/' in
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
              message = error state.line "" "Undetermined string.";
              data = (state, tokens);
            }
        else
          let state, _ = advance state in
          let value = String.sub source (state.start + 1) (state.current - 1) in
          let tokens = add_token state (StrLiteral value) STRING in
          Ok (state, tokens)
    | c ->
        let is_digit c = c >= '0' && c <= '9' in
        let is_alpha c =
          (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
        in
        let is_alpha_numeric c = is_digit c || is_alpha c in

        (* Number literals *)
        if is_digit c then
          let rec loop_number c state =
            if is_digit c then
              let state, _ = advance state in
              let peek = peek state in
              loop_number peek state
            else state
          in
          let state = loop_number c state in
          (* Look for a fractional part *)
          if peek state == '.' && is_digit (peek_next state) then
            (* Consume the "." *)
            let state, _ = advance state in
            let state = loop_number c state in
            let value =
              Float.of_string
                (String.sub source state.start (state.current - 1))
            in
            let tokens = add_token state (NumLiteral value) Token.NUMBER in
            Ok (state, tokens)
          else
            let tokens = add_token state None Token.NUMBER in
            Ok (state, tokens)
        else if is_alpha c then
          (* Reserved words and Identifiers *)
          (* var *)
          let rec identifier state =
            let c = peek state in
            if is_alpha_numeric c then
              let state, _ = advance state in
              identifier state
            else state
          in
          let state = identifier state in
          let text = String.sub source state.start state.current in
          let token_type =
            try
              let token_type = Hashtbl.find identifiers text in
              Some token_type
            with Not_found -> None
          in

          let tokens =
            (*
              If a reserved word was found on the token_type Hashtbl
              we add it to "tokens" else the text we found it is a "identifier"
              established by the user
            *)
            match token_type with
            | Some t -> add_token state (StrLiteral text) t
            | None -> add_token state (StrLiteral text) IDENTIFIER
          in

          Ok (state, tokens)
        else
          Error
            {
              message = error state.line "" "Unexpected char.";
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
    | Ok (state, tokens) -> scan_tokens state source tokens identifiers
    | Error e ->
        print_endline e.message;
        let _, tokens = e.data in
        tokens
