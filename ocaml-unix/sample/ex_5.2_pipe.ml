open Unix

(** (1) The representation used by below functions is unspecified,
    but it is guaranteed to be platform-independent for a particular version of OCaml.

    (2) These functions use buffered I/O (via channel), fewer system calls are performed,
    which results in better performance.
*)

(** read integer from channel in four-byte binary representation *)
let input_int = input_binary_int

(** write integer to channel in four-byte binary representation *)
let output_int = output_binary_int

(** enumerate integers from 2 to k *)
let generate k output =
  let rec gen m =
    output_int output m ;
    if m < k then gen (m + 1)
  in
  gen 2

let print_prime n = print_int n ; print_newline ()

(** Reads [count] prime numbers on [input], and eliminates multiples of the primes already read.
    These [count] primes are displayed as soon as they are read and we return them in a list.
*)
let read_first_primes input count =
  let rec read_primes first_primes count =
    if count <= 0 then first_primes (* base case *)
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
        (* Child *)
        (* starts to filter the output of the pipe *)
        close fd_out ;
        filter (in_channel_of_descr fd_in)
    (* it is possible but very risky to create more than one in_channel on the same descriptor. *)
    | p ->
        (* Parent *)
        (* reads numbers on its input,
           and writes each one to the pipe,
           if it is not a multiple of one of the 1000 primes it initially read. *)
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
      (* Child *)
      (* If argv is not given, all processes continue indefinitely until
       one or more are killed. The death of a process results in the
       death of its child as described above. It also close the output
       of the pipe connected to its parent. This will in turn kill the
       parent at the next write on the pipe. The parent will receive a
       sigpipe signal whose default handler terminates the process. *)
      close fd_out ;
      filter (in_channel_of_descr fd_in)
  | p ->
      (* Parent *)
      (* Generators.  When argv is given, parent will terminate first,
       and close the descriptor on the input of the pipe connected to
       its child. Since OCaml empties the buffers of descriptors open
       in write mode when a process tops, the child process wil read
       the last integer provided by the parent. After that, the child
       also stops. Thus, in this program, children become orphaned and
       are temporarily attached to the process init before they die in
       turn.  *)
      close fd_in ;
      generate len (out_channel_of_descr fd_out)

let () = handle_unix_error sieve ()
