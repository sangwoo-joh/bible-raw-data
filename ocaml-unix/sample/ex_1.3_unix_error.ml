open Unix

let handle_unix_error f arg =
  try f arg
  with Unix_error (err, fun_name, arg) ->
    prerr_string Sys.argv.(0) ;
    prerr_string ": \"" ;
    prerr_string fun_name ;
    prerr_string "\" failed" ;
    if String.length arg > 0 then (
      prerr_string " on \"" ; prerr_string arg ; prerr_string "\"" ) ;
    prerr_string ": " ;
    prerr_endline (error_message err) ;
    exit 2
