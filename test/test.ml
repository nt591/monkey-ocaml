open Monkey
include Lexer
include Token
(* A module with functions to test *)
let test_list_concat () =
  Alcotest.(check (list int))
    "same lists" [ 1; 2; 3 ]
    (List.append [ 1 ] [ 2; 3 ])

let token_testable = Alcotest.testable Token.pretty_print Token.tokens_eq

let source_test_string =
  "let five = 5;

  let ten = 10;

  let add = fn(x, y) {
    x + y;
  };

  let result = add(five, ten);

	!-/*5;
	5 < 10 > 5;

	if (5 < 10) {
		return true;
	} else {
		return false;
	}


	10 == 10;
	10 != 9;
  "

let source_test_expected =
  [
    (* // let five = 5 *)
		Token.LET;
		Token.IDENT "five";
		Token.ASSIGN;
		Token.INT "5";
		Token.SEMICOLON;

		(* // let ten = 10 *)
		Token.LET;
		Token.IDENT "ten";
		Token.ASSIGN;
		Token.INT "10";
		Token.SEMICOLON;

		(* // function definition *)
		Token.LET;
		Token.IDENT "add";
		Token.ASSIGN;
		Token.FUNCTION;
		Token.LPAREN;
		Token.IDENT "x";
		Token.COMMA;
		Token.IDENT "y";
		Token.RPAREN;
		Token.LBRACE;
		Token.IDENT "x";
		Token.PLUS;
		Token.IDENT "y";
		Token.SEMICOLON;
		Token.RBRACE;
		Token.SEMICOLON;

		(* // result definition *)
		Token.LET;
		Token.IDENT "result";
		Token.ASSIGN;
		Token.IDENT "add";
		Token.LPAREN;
		Token.IDENT "five";
		Token.COMMA;
		Token.IDENT "ten";
		Token.RPAREN;
		Token.SEMICOLON;

		(* // junk *)
		Token.BANG;
		Token.MINUS;
		Token.SLASH;
		Token.ASTERISK;
		Token.INT "5";
		Token.SEMICOLON;
		Token.INT "5";
		Token.LT;
		Token.INT "10";
		Token.GT;
		Token.INT "5";
		Token.SEMICOLON;

		(* // if/else/return *)
		Token.IF;
		Token.LPAREN;
		Token.INT "5";
		Token.LT;
		Token.INT "10";
		Token.RPAREN;
		Token.LBRACE;
		Token.RETURN;
		Token.TRUE;
		Token.SEMICOLON;
		Token.RBRACE;
		Token.ELSE;
		Token.LBRACE;
		Token.RETURN;
		Token.FALSE;
		Token.SEMICOLON;
		Token.RBRACE;

		(* // double character operators *)
		Token.INT "10";
		Token.EQ;
		Token.INT "10";
		Token.SEMICOLON;
		Token.INT "10";
		Token.NOT_EQ;
		Token.INT "9";
		Token.SEMICOLON;
    (* sentinel *)
    Token.EOF;
  ]

let test_lexer_delimiters () =
  Alcotest.(check (list token_testable))
    "same token types" [
        Token.ASSIGN
      ; Token.PLUS
      ; Token.LPAREN
      ; Token.RPAREN
      ; Token.LBRACE
      ; Token.RBRACE
      ; Token.COMMA
      ; Token.SEMICOLON
      ; Token.EOF
      ]
      (Lexer.generate_tokens "=+(){},;")


let test_lexer_source () =
  Alcotest.(check (list token_testable))
    "source code can lex" source_test_expected
      (Lexer.generate_tokens source_test_string)

(* Run it *)
let () =
  Alcotest.run "Lexer"
    [
      ( "list-delimiters",
        [ Alcotest.test_case "first case" `Slow test_lexer_delimiters ] );

      ( "Lexer.generate_tokens",
        [ Alcotest.test_case "source code" `Slow test_lexer_source ] )
    ]