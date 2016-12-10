let link xtndOut ast compiler outputFile =
  let tmpFilename = Filename.temp_file "" ".ll"
  and getExterns (_,_,extern) =
    Ast.StringMap.fold
      (fun key value store -> value.Ast.extern_fn_libname :: store)
      extern
      [] in
  let tmpChan = open_out tmpFilename in
  output_string tmpChan xtndOut;
  let call = (String.concat " " (compiler :: "-o" :: outputFile :: tmpFilename :: (getExterns ast))) in
  let res = print_endline call; Sys.command call in
  close_out tmpChan; Sys.remove tmpFilename; if res == 0 then () else raise Not_found
