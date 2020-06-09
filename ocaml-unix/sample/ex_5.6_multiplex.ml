open Unix

let rec really_read fd buf start length =
  if length <= 0 then ()
  else
    match read fd buf start length with
    | 0 ->
        raise End_of_file
    | n ->
        really_read fd buf (start + n) (length - n)

let buffer = Bytes.create 258

let multiplex chan inputs outputs =
  let input_fds = chan :: Array.to_list inputs in
  try
    while true do
      let ready_fds, _, _ = select input_fds [] [] (-1.0) in
      for i = 0 to Array.length inputs - 1 do
        if List.mem inputs.(i) ready_fds then (
          let n = read inputs.(i) buffer 2 255 in
          buffer.[0] <- char_of_int i ;
          buffer.[1] <- char_of_int n ;
          ignore (write chan buffer 0 (n + 2)) ;
          () )
      done ;
      if List.mem chan ready_fds then (
        really_read chan buffer 0 2 ;
        let i = int_of_char (Bytes.unsafe_get buffer 0) in
        let n = int_of_char (Bytes.unsafe_get buffer 1) in
        if n = 0 then close outputs.(i)
        else (
          really_read chan buffer 0 n ;
          ignore (write outputs.(i) buffer 0 n) ;
          () ) )
    done
  with End_of_file -> ()
