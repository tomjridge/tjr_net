(** Unix instance *)

open Unix

module Monad = struct
  (* NOTE the question is how to propagate errors from the lower
     level; ideally this should happen invisibly, hence the exn in
     the following; however, catch and finally should function with
     these errors *)
  type 'a m = 'a
  let return x = x
  let bind a b = b a
end
include Monad

module Pvt = struct
  module S = struct
    type fd = Unix.file_descr
    include Monad
  end
  module G = Generic.Make(S)
  open G

  type unix_error = Unix.error * string * string

  (* NOTE this traps exceptions from the lower level and passes them into the monad *)
  let wrap f = 
    try Ok(f ()) with 
    | Unix.Unix_error (e,s1,s2) -> Error `EOTHER
  (* TODO refine this to match the errors eg socket_err *)
  (* NOTE other exceptions are not caught *)

  let log_ = Generic.log_

  let wrap1 s f = fun a -> wrap @@ fun () -> log_ s; f a
  let wrap2 s f = fun a b -> wrap @@ fun () -> log_ s; f a b 
  let wrap3 s f = fun a b c -> wrap @@ fun () -> log_ s; f a b c 
  let wrap4 s f = fun a b c d -> wrap @@ fun () -> log_ s; f a b c d

  let net_ops = {
    socket=(wrap3 "socket" socket);
    setsockopt=(setsockopt);
    bind_=(wrap2 "bind" Unix.bind);
    listen=(wrap2 "listen" listen);
    accept=(wrap1 "accept" accept);
    getpeername=(getpeername);
    close=(fun c -> try close c with Unix.Unix_error _ -> ());
    connect=(wrap2 "connect" connect);
    write=(wrap4 "write" write);
    read=(wrap4 "read" read);
  }
  
  let msg_ops = G.mk_msg_lib ~ops:net_ops
end

let unix_msg_ops = Pvt.msg_ops
let msg_ops = unix_msg_ops

let Pvt.G.{ listen_accept; connect; send_string; send_strings; recv_string; recv_strings } =
  msg_ops
