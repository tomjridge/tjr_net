(* open Lwt *)
open Tjr_connection
open Tjr_connection.Lwt_

let pid = Unix.getpid () |> string_of_int

let main () = 
  Lwt_.connect ~quad:Shared.sender >>= fun conn ->
  let rec loop i () = 
    (* dummy message: pid and a counter *)
    let msg = pid ^" "^(string_of_int i) in
    (if i mod 100 = 0 then print_endline msg else ());
    send_string ~conn msg >>= function 
    | `Ok ->
      recv_string ~conn >>= fun s ->
      loop (i+1) ()
    | _ -> failwith __LOC__
  in
  loop 0 ()

let _ = Lwt_main.run @@ main()
