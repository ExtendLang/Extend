open OUnit2;;

let str1 = "{\"Program\": {\"Imports\": [],\"Globals\": [],\"Functions\": [{\"Name\": \"main\",\"Params\": [],\"Stmts\": [{\"Vardecl\": {\"Dimensions\": {\"d1\": null, \"d2\": null},\"Initializations\": [{\"VarName\": \"foo\", \"expr\": {\"Id\": \"bar\"}}]}}],\"ReturnVal\": {\"Dimensions\": {\"d1\": {\"LitInt\":2}, \"d2\": {\"LitInt\":3}}, \"expr\": {\"Id\": \"baz\"}}}]}}";;
let str2 = "{\"Program\": {\"Imports\": [],\"Globals\": [],\"Functions\": []}}";;
let test1 test_ctxt = assert_equal (Jsonify.jsonify (Lexing.from_string "[2,3] main() {foo := bar; return baz;}")) (str1);;
let test2 test_ctxt = assert_equal (Jsonify.jsonify (Lexing.from_string "")) (str2);;

let suite = 
"suite">:::
["test1">:: test1;
 "test2">:: test2;]

let () =
  run_test_tt_main suite
;;
