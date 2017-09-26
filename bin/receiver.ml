open Lwt
open Lwt_unix
open Tjr_connection

let main () = 
  listen_accept ~quad:Shared.recvr >>= fun conn ->
  let rec loop i () =
    recv_string ~conn >>= fun msg ->
    (if i mod 100 = 0 then print_endline msg else ());
    send_string ~conn ~string_:msg >>= fun () ->
    loop (i+1) ()
  in
  loop 0 ()

let _ = Lwt_main.run @@ main()
