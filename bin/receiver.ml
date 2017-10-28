open Tjr_connection
open Unix_

let log_ x = ()

let main () = 
  log_ __LOC__;
  listen_accept ~quad:Shared.recvr >>= function
  | `Listen_accept_incorrect_peername -> failwith __LOC__
  | `Connection conn ->
    log_ __LOC__;
    let rec loop i () =
      recv_string ~conn >>= function
      | `Ok msg -> (
          (if i mod 100 = 0 then print_endline msg else ());
          send_string ~conn msg >>= function
          | `Ok ->
            loop (i+1) ()
          | _ -> failwith __LOC__)
      | _ -> failwith __LOC__
    in
    loop 0 ()

let _ = main()|> function
  | Ok x -> ()
  | Error (e,s1,s2) -> raise (Unix.Unix_error(e,s1,s2))
