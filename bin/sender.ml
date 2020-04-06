open Lwt.Infix
open Tjr_net.Lwt_

let pid = Unix.getpid () |> string_of_int

let main () = 
  Lwt_.connect Shared.sender >>= function
  | Error _ -> failwith __LOC__
  | Ok conn ->
    let rec loop i () = 
      (* dummy message: pid and a counter *)
      let msg = pid ^" "^(string_of_int i) in
      (if i mod 100 = 0 then print_endline msg else ());
      send_string conn msg >>= function 
      | Error () -> failwith __LOC__
      | Ok () ->
        recv_string conn >>= function
        | Error () -> failwith __LOC__
        | Ok s ->
          loop (i+1) ()
    in
    loop 0 ()

let _ = Lwt_main.run @@ main()
