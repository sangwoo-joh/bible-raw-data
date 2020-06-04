open Unix

let input_int = input_binary_int

let output_int = output_binary_int

let generate k output =
  let rec gen m =
    output_int output m ;
    if m < k then gen (m + 1)
  in
  gen 2

let print_prime n = print_int n ; print_newline ()

let read_first_primes input count =
  let rec read_primes first_primes count =
    if count <= 0 then first_primes
    else
      let n = input_int input in
      if List.exists (fun m -> n mod m = 0) first_primes then
        read_primes first_primes count
      else (
        print_prime n ;
        read_primes (n :: first_primes) (count - 1) )
  in
  read_primes [] count

let rec filter input =
  try
    let first_primes = read_first_primes input 1000 in
    let fd_in, fd_out = pipe () in
    match fork () with
    | 0 ->
        close fd_out ;
        filter (in_channel_of_descr fd_in)
    | p ->
        close fd_in ;
        let output = out_channel_of_descr fd_out in
        while true do
          let n = input_int input in
          if List.exists (fun m -> n mod m = 0) first_primes then ()
          else output_int output n
        done
  with End_of_file -> ()

let sieve () =
  let len = try int_of_string Sys.argv.(1) with _ -> max_int in
  let fd_in, fd_out = pipe () in
  match fork () with
  | 0 ->
      close fd_out ;
      filter (in_channel_of_descr fd_in)
  | p ->
      close fd_in ;
      generate len (out_channel_of_descr fd_out)

let () = handle_unix_error sieve ()
