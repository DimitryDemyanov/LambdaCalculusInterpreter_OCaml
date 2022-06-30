
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;
open String;;
open Str;;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop (vctx, tctx) =
    print_string ">> ";
    flush stdout;
    try
      let read_input = let rec read_line_rec strings input_line =
        if (Str.string_match (Str.regexp ".*;") input_line 0) then (String.concat " " (strings@[input_line]))
        else (read_line_rec (strings@[input_line]) (read_line()))
      in (read_line_rec [] (read_line())) in
      let c = s token (from_string (read_input)) in 
      loop (execute (vctx, tctx) c)
    with
       Lexical_error ->
         print_endline "lexical error";
         loop (vctx, tctx)
     | Parse_error ->
         print_endline "syntax error";
         loop (vctx, tctx)
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop (vctx, tctx)
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop (emptyvctx, emptytctx)
  ;;

top_level_loop ()
;;

