module StringSet = Set.Make(String)
let link xtndOut ast compiler outputFile =
  let tmpFilenameLL = Filename.temp_file "" ".ll"
  and tmpFilenameC = Filename.temp_file "" ".o"
  and getExterns (_,_,extern) =
    StringSet.elements
      (Ast.StringMap.fold
        (fun key value store -> StringSet.add value.Ast.extern_fn_libname store)
        extern
        StringSet.empty) in
  let tmpChan = open_out tmpFilenameLL in
  output_string tmpChan xtndOut; close_out tmpChan;
  let call1 = (String.concat " " ("llc-3.8" :: "-filetype=obj" :: tmpFilenameLL :: "-o" :: tmpFilenameC :: []))
  and call2 = (String.concat " " (compiler :: "-o" :: outputFile :: tmpFilenameC :: (getExterns ast) @ ["runtime.o"])) ^ " -Llib/gdchart0.94b/gd1.3 -lgd -lm" in
  let resc1 = Sys.command call1 in
  if resc1 == 0 then (
    Sys.remove tmpFilenameLL;
    let resc2 = Sys.command call2 in
      Sys.remove tmpFilenameC;
      if resc2 == 0 then () else raise Not_found
    )
  else (Sys.remove tmpFilenameC;raise Not_found)
