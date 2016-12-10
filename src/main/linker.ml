module StringSet = Set.Make(String)
let link xtndOut ast compiler outputFile =
  let tmpFilename = Filename.temp_file "" ".ll"
  and getExterns (_,_,extern) =
    StringSet.elements
      (Ast.StringMap.fold
        (fun key value store -> StringSet.add value.Ast.extern_fn_libname store)
        extern
        StringSet.empty) in
  let tmpChan = open_out tmpFilename in
  output_string tmpChan xtndOut; close_out tmpChan;
  let call = (String.concat " " (compiler :: "-o" :: outputFile :: tmpFilename :: (getExterns ast))) ^ " -lm" in
  let res = Sys.command call in
  Sys.remove tmpFilename; if res == 0 then () else raise Not_found
