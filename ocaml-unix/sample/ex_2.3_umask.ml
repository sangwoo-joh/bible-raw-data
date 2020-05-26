let get_mask () =
  let m = Unix.umask 0 in
  ignore (Unix.umask m) ;
  m
