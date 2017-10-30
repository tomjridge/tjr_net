open Tjr_connection
open Unix_

let log_ x = ()

let main () = 
  log_ __LOC__;
  Printf.printf "Receiver listening for connection\n"; flush_all();
  listen_accept ~quad:Shared.recvr >>= function
  | Error _ -> failwith __LOC__
  | Ok conn ->
    log_ __LOC__;
    let rec loop i () =
      recv_string ~conn >>= function
      | Error () -> failwith __LOC__
      | Ok msg -> (
          (if i mod 100 = 0 then print_endline msg else ());
          send_string ~conn msg >>= function
          | Error () -> failwith __LOC__
          | Ok () -> loop (i+1) ())
    in
    loop 0 ()

let _ = main ()
