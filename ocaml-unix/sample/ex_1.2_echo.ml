let echo () =
  let len = Array.length Sys.argv in
  if len > 1 then (
    print_string Sys.argv.(1) ;
    for i = 2 to len - 1 do
      print_char ' ' ;
      print_string Sys.argv.(i)
    done ;
    print_newline () )

let () = echo ()
