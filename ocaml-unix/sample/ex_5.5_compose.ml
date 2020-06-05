open Sys
open Unix

let compose () =
  let n = Array.length Sys.argv - 1 in
  for i = 1 to n - 1 do
    let fd_in, fd_out = pipe () in
    match fork () with
    | 0 ->
        (* parent *)
        dup2 fd_out stdout ;
        close fd_out ;
        close fd_in ;
        execv "/bin/sh" [|"/bin/sh"; "-c"; Sys.argv.(i)|]
    | _ ->
        dup2 fd_in stdin ; close fd_out ; close fd_in
  done ;
  match fork () with
  | 0 ->
      execv "/bin/sh" [|"bin/sh"; "-c"; Sys.argv.(n)|]
  | _ ->
      let rec wait_for_children retcode =
        try
          match wait () with
          | pid, WEXITED n ->
              wait_for_children (retcode lor n)
          | pid, _ ->
              wait_for_children 127
        with Unix_error (ECHILD, _, _) -> retcode
      in
      exit (wait_for_children 0)

let () = handle_unix_error compose ()
