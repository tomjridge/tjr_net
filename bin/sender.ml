open Lwt
open Tjr_connection


let pid = Unix.getpid () |> string_of_int

let main () = 
  connect ~quad:Shared.sender >>= fun conn ->
  let rec loop i () = 
    (* dummy message: pid and a counter *)
    let msg = pid ^" "^(string_of_int i) in
    (if i mod 100 = 0 then print_endline msg else ());
    send_string ~conn ~string_:msg >>= fun () ->
    recv_string ~conn >>= fun s ->
    loop (i+1) ()
  in
  loop 0 ()

let _ = Lwt_main.run @@ main()
