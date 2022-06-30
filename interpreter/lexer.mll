
{
  open Parser;;
  exception Lexical_error;; 
}

rule token = parse
    [' ' '\t'] 			{ token lexbuf }
  | "lambda"    		{ LAMBDA }
  | "L"         		{ LAMBDA }
  | "true"      		{ TRUE }
  | "false"    			{ FALSE }
  | "if"       			{ IF }
  | "then"      		{ THEN }
  | "else"      		{ ELSE }
  | "succ"      		{ SUCC }
  | "pred"      		{ PRED }
  | "iszero"    		{ ISZERO }
  | "first"|"fst"       { FIRST }
  | "second"|"snd"      { SECOND }
  | "let"       		{ LET }
  | "in"        		{ IN }
  | "Bool"      		{ BOOL }
  | "Nat"       		{ NAT }
  | "letrec"			{ LETREC }
  | "concat"			{ CONCAT }
  | '('         		{ LPAREN }
  | ')'         		{ RPAREN }
  | '{'         		{ LTUPLA }
  | '}'         		{ RTUPLA }
  | '*'         		{ ASTERISCO }
  | '.'         		{ DOT }
  | '='         		{ EQ }
  | ':'         		{ COLON }
  | '^'				    { CIRCUNFLEJO }
  | ","					{ COMA }
  | "->"        		{ ARROW }
  | ";"         		{ EOEXPRESION }
  | ['0'-'9']+  		{ INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | '"'[^ '"' ';' '\n']*'"'           { let s = Lexing.lexeme lexbuf in TSTRING (String.sub s 1 (String.length s - 2)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*   { STRINGV (Lexing.lexeme lexbuf) }
  | eof         		{ EOF }
  | _           		{ raise Lexical_error } 

