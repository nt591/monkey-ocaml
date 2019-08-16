open Monkey
include Lexer
include Token
(* A module with functions to test *)
let test_list_concat () =
  Alcotest.(check (list int))
    "same lists" [ 1; 2; 3 ]
    (List.append [ 1 ] [ 2; 3 ])

let token_testable = Alcotest.testable Token.pretty_print (=)

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
(* Run it *)
let () =
  Alcotest.run "Utils"
    [
      ( "list-delimiters",
        [ Alcotest.test_case "first case" `Slow test_lexer_delimiters ] )
    ]