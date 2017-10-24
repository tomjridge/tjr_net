open Lwt
open Tjr_connection
open Lwt_

let main () = 
  listen_accept ~quad:Shared.recvr >>= function
  | `Net_err e -> raise e
  | `Error_incorrect_peername -> failwith __LOC__
  | `Connection conn ->
    let rec loop i () =
      recv_string ~conn >>= fun msg ->
      (if i mod 100 = 0 then print_endline msg else ());
      send_string ~conn msg >>= fun () ->
      loop (i+1) ()
    in
    loop 0 ()

let _ = Lwt_main.run @@ main()
