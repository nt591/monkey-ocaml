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
    | MINUS
    | BANG
    | ASTERISK
    | SLASH
    | LT
    | GT
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
    | TRUE
    | FALSE
    | IF
    | ELSE
    | RETURN
    | EQ
    | NOT_EQ


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
  | MINUS -> "MINUS"
  | BANG -> "BANG"
  | ASTERISK -> "ASTERISK"
  | SLASH -> "SLASH"
  | LT -> "LT"
  | GT -> "GT"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | RETURN -> "RETURN"
  | EQ -> "EQ"
  | NOT_EQ -> "NOT_EQ"

  (* TODO, convert from association list to map *)
  let string_to_token = [
    "fn", FUNCTION;
    "let", LET;
    "if", IF;
    "else", ELSE;
    "true", TRUE;
    "false", FALSE;
    "return", RETURN;
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