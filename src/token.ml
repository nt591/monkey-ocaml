module Token =  struct
  include Fmt
  type token_type =
    | ILLEGAL
    | EOF
    (* Identifiers and literals *)
    | IDENT
    | INT
    (* Operators *)
    | ASSIGN
    | PLUS
    (* -- Delimiters *)
    | COMMA
    | SEMICOLON
    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    (* -- Keywords *)
    | FUNCTION
    | LET


  type token = {
    t_type : token_type;
    literal : string;
  }

  let token_to_string = function
  | ILLEGAL -> "ILLEGAL"
  | EOF -> "EOF"
  | IDENT -> "IDENT"
  | INT -> "INT"
  | ASSIGN -> "ASSIGN"
  | PLUS -> "PLUS"
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | FUNCTION -> "FUNCTION"
  | LET -> "LET"

  let pretty_print ppf tok = Fmt.pf ppf "Testing token %s" (token_to_string tok)

  let new_token tok_type ch = {
    t_type = tok_type;
    literal = ch;
  }
end