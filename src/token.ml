module Token =  struct
  include Fmt
  type token_type =
    | ILLEGAL
    | EOF
    (* Identifiers and literals *)
    | IDENT of string
    | INT of string
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
  | IDENT a -> "IDENT " ^ a
  | INT a -> "INT " ^ a
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

  let string_to_token = [
    "fn", FUNCTION;
    "let", LET;
  ]

  let lookupIdent str = try
    List.assoc str string_to_token
    with Not_found -> IDENT str

  let tokens_eq tok_a tok_b = match (tok_a, tok_b) with
  | IDENT a, IDENT b -> a = b
  | tok_a, tok_b -> tok_a = tok_b

  let pretty_print ppf tok = Fmt.pf ppf "Token %s" (token_to_string tok)

  let new_token tok_type ch = {
    t_type = tok_type;
    literal = ch;
  }
end