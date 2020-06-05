open Sys
open Unix

let compose () =
  let n = Array.length Sys.argv - 1 in
  for i = 1 to n - 1 do
    let fd_in, fd_out = pipe () in
    match fork () with
    | 0 ->
        (* child *)
        dup2 fd_out stdout (* connect pipe's input to stdout *) ;
        close fd_out ;
        close fd_in ;
        execv "/bin/sh" [|"/bin/sh"; "-c"; Sys.argv.(i)|]
    | _ ->
        (* parent *)
        dup2 fd_in stdin (* connect pipe's output to stdin *) ;
        close fd_out ;
        close fd_in
  done ;
  (* for the last command, there's no need to create a new pipe *)
  match fork () with
  | 0 ->
      (* child *)
      execv "/bin/sh" [|"bin/sh"; "-c"; Sys.argv.(n)|]
  | _ ->
      (* parent *)
      (* wait for children to terminate *)
      let rec wait_for_children retcode =
        try
          match wait () with
          | pid, WEXITED n ->
              wait_for_children (retcode lor n)
          | pid, _ ->
              wait_for_children 127
        with Unix_error (ECHILD (* no child to wait for *), _, _) -> retcode
      in
      exit (wait_for_children 0)

let () = handle_unix_error compose ()
