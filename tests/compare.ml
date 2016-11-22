let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s);;

let dir = "./samples" in
  let children = Sys.readdir dir in
    Array.iter (
    fun s -> let in_file = (dir ^ "/" ^ s) in
      try
        OUnit2.assert_equal ((Ast.string_of_program (Transform.create_ast in_file)) ^ "\n") (load_file (dir ^ "_comp/" ^ s ^ ".out"))
      with
        e -> (print_endline ("Failure in " ^ s) ; raise e)
    ) children;;
