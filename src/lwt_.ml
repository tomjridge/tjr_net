(** Lwt instance *)

open Lwt
open Lwt_unix

module Pvt = struct
  module S = struct
    type fd = Lwt_unix.file_descr
    type 'a m = 'a Lwt.t
    let return,bind = Lwt.(return,bind)                      
  end 
  module G = Generic.Make(S)
  open G

  let wrap f = catch (fun () -> f () >>= fun x -> return (Ok x)) (fun e -> return (Error `EOTHER))

  (** Underlying network operations *)
  let net_ops = {
    socket=(fun a b c -> wrap (fun () -> socket a b c|>return));
    setsockopt=(fun a b c -> setsockopt a b c |> return);
    bind_=(fun a b -> wrap (fun () -> Lwt_unix.bind a b));
    listen=(fun a b -> wrap (fun () -> listen a b|>return));
    accept=(fun a -> wrap (fun () -> accept a));
    getpeername=(fun x -> getpeername x |>return);
    close=(fun c -> catch (fun () -> close c) (fun e -> return ()));
    connect=(fun a b -> wrap (fun () -> connect a b));
    write=(fun a b c d -> wrap (fun () -> write a b c d));
    read=(fun a b c d -> wrap (fun () -> read a b c d));
  }

  let msg_ops = G.mk_msg_lib ~ops:net_ops
  

  (* TODO refine errors *)
end

let lwt_msg_ops = Pvt.msg_ops
let msg_ops = lwt_msg_ops

let Pvt.G.{ listen_accept; connect; send_string; send_strings; recv_string; recv_strings } =
  msg_ops

