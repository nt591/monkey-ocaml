module Lexer = struct
  include Token

  type lexer = {
    input : string;
    position : int;
    read_position : int;
    ch : char;
  }

  (* https://stackoverflow.com/questions/36993643/idiomatic-way-to-include-a-null-character-byte-in-a-string-in-ocaml *)
  let null_byte = '\x00'

  let peek_char lexer =
    if lexer.read_position >= String.length(lexer.input) then null_byte else String.get lexer.input lexer.read_position

  let read_char lexer =
    let read_to_end = lexer.read_position >= String.length(lexer.input) in
    let new_ch = match read_to_end with
    | true -> null_byte
    | false -> String.get lexer.input lexer.read_position
    in {lexer with position = lexer.read_position; read_position = lexer.read_position + 1; ch = new_ch}


  let new_lexer input_string =
    let lexer = {
      input = input_string;
      position = 0;
      read_position = 0;
      ch = null_byte;
    } in
    read_char lexer

  let is_letter c = 'a' <= c && c <= 'z' || 'A' <= c  && c <= 'Z' || c == '_'

  let is_digit c = '0' <= c && c <= '9'

  let read_type fn lexer =
    let position = lexer.position in
    let rec read lex = if (fn lex.ch) then lex |> read_char |> read else lex in
    let updated_lex = read lexer in
    (updated_lex, (String.sub updated_lex.input position (updated_lex.read_position - position - 1)))

  let read_identifier = read_type is_letter

  let read_number = read_type is_digit

  let rec next_char lexer =
    let skip l = l |> read_char |> next_char in
    let double_read l = l |> read_char |> read_char in
    match lexer.ch with
    | '=' -> if (peek_char lexer) = '='
      then (double_read lexer , Token.EQ)
      else (read_char lexer, Token.ASSIGN)
    | ';' -> (read_char lexer, Token.SEMICOLON)
    | '(' -> (read_char lexer, Token.LPAREN)
    | ')' -> (read_char lexer, Token.RPAREN)
    | ',' -> (read_char lexer, Token.COMMA)
    | '+' -> (read_char lexer, Token.PLUS)
    | '{' -> (read_char lexer, Token.LBRACE)
    | '}' -> (read_char lexer, Token.RBRACE)
    | '-' -> (read_char lexer, Token.MINUS)
    | '!' -> if (peek_char lexer) = '='
        then (double_read lexer, Token.NOT_EQ)
        else (read_char lexer, Token.BANG)
    | '*' -> (read_char lexer, Token.ASTERISK)
    | '/' -> (read_char lexer, Token.SLASH)
    | '<' -> (read_char lexer, Token.LT)
    | '>' ->( read_char lexer, Token.GT)
    | ' ' -> skip lexer
    | '\t' -> skip lexer
    | '\n' -> skip lexer
    | '\r' -> skip lexer
    | '\x00' -> (lexer, Token.EOF)
    | c ->
        if (is_letter c) then
          let (new_lex, tok_literal) =  read_identifier lexer in
          (new_lex, Token.lookupIdent tok_literal)
        else begin
          match is_digit c with
          | true ->
            let (new_lex, tok_literal) = read_number lexer in
            (new_lex, Token.INT tok_literal)
          | false -> (read_char lexer, Token.ILLEGAL)
        end

  let generate_tokens input_string =
    let lexer = new_lexer input_string in
    let rec gen lxr tokens =
      match next_char lxr with
      | (_, Token.EOF) -> List.rev_append tokens [Token.EOF]
      | (l, tok) -> gen l (tok :: tokens)
    in gen lexer []

end