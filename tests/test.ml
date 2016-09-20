open OUnit2;;

let test1 test_ctxt = assert_equal "Hello world!" (Hello.hello());;
let test2 test_ctxt = assert_equal "Hey, it's Ishaan!" (Hello.greet());;

let suite = 
"suite">:::
["test1">:: test1;
 "test2">:: test2;]

let () =
  run_test_tt_main suite
;;
