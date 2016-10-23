let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s);;

let dir = "./samples" in
  let children = Sys.readdir dir in
    Array.iter (
      fun s -> let in_channel = open_in (dir ^ "/" ^ s) in
        OUnit2.assert_equal ((Jsonify.jsonify (Lexing.from_channel in_channel)) ^ "\n") (load_file (dir ^ "_comp/" ^ s ^ ".out"))
    ) children;;

print_endline "Hi";
