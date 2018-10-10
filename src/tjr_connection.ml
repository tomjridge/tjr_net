
let log_ s = ()


(* aux -------------------------------------------------------------- *)

(* int <-> byte_x4 conversion *)

(* assert that is always checked *)
let assert_ b = if b then () else assert false

let base=256

let strip_byte i = i/base,i mod base

let co_strip_byte b i = i*base+b

(* strip n bytes from i and write into buf *)
let i2bs ~buf ~off ~i ~n =
  assert_ (i>=0);
  let rec f ~off ~i ~n = n |> function
    | 0 -> ()
    | _ ->
      i |> strip_byte |> fun (i,b) ->
      Bytes.set buf off (b |> Char.chr);
      f ~off:(off+1) ~i ~n:(n-1)
  in
  f ~off ~i ~n

(* read n bytes from buf (going down from off!) and return int *)
let bs2i ~buf ~off ~n = 
  let rec f ~off ~i ~n = n |> function
    | 0 -> i
    | _ -> 
      Bytes.get buf off |> Char.code |> fun b ->
      co_strip_byte b i |> fun i ->
      f ~off:(off-1) ~i ~n:(n-1)
  in
  f ~off ~i:0 ~n


let _ = assert (
  let i = 123456 in
  Bytes.create 4 |> fun buf ->
  i2bs ~buf ~off:0 ~i ~n:4;
  bs2i ~buf ~off:3 ~n:4 = i)



(* address types -------------------------------------------------------- *)

include struct
open Unix (* for PF_NET SOCK_STREAM etc *)

(* Lwt_unix has file_descr <> Unix.file_descr *)

(* type 'a conn = File_descr of 'a *)
type ip = inet_addr
type port = int
type ipp = sockaddr (*  expect ADDR_INET ip * port *)
type quad = { local:ipp; remote: ipp }
end


(* functors ---------------------------------------------------------- *)

module type FILE_DESCR = sig type file_descr end

module type MONAD = sig
  type 'a m
  val return : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
end 

(* NOTE these types identify some of the errors; to be refined
   later as needed *)
type socket_err = [ `ERESOURCE | `EOTHER ]
type bind_err = [ `EADDRINUSE | `EOTHER ]
type listen_err = [ `EOTHER ]
type accept_err = [ `EOTHER ]
type connect_err = [ `EREFUSED | `EHOSTUNREACH | `EOTHER ]
type read_err = [ `EOTHER ]
type write_err = [ `EOTHER ]

module type NET_OPS = sig
  open Unix
  include FILE_DESCR
  include MONAD

  (* NOTE errors from these lower-level functions are captured in the
     monad; but they can be dealt with at the higher level by using
     catch and finally *)

  type net_ops = {
    socket: socket_domain -> socket_type -> int -> (file_descr,socket_err)result m;
    setsockopt: file_descr -> socket_bool_option -> bool -> unit m;
    bind_: file_descr -> sockaddr -> (unit,bind_err)result m;
    listen: file_descr -> int -> (unit,listen_err)result m;
    accept: file_descr -> (file_descr * sockaddr,accept_err)result m;
    getpeername: file_descr -> sockaddr m;  (* option ?*)
    close: file_descr -> unit m;  
    connect: file_descr -> sockaddr -> (unit,connect_err)result m;
    write: file_descr -> bytes -> int -> int -> (int,write_err)result m;
    read: file_descr -> bytes -> int -> int -> (int,read_err)result m;
  }
end

module Make_net_ops_type(FD:FILE_DESCR)(M:MONAD) = struct
  open Unix
  open FD
  open M

  type net_ops = {
    socket: socket_domain -> socket_type -> int -> (file_descr,socket_err)result m;
    setsockopt: file_descr -> socket_bool_option -> bool -> unit m;
    bind_: file_descr -> sockaddr -> (unit,bind_err)result m;
    listen: file_descr -> int -> (unit,listen_err)result m;
    accept: file_descr -> (file_descr * sockaddr,accept_err)result m;
    getpeername: file_descr -> sockaddr m;  (* option ?*)
    close: file_descr -> unit m;  
    connect: file_descr -> sockaddr -> (unit,connect_err)result m;
    write: file_descr -> bytes -> int -> int -> (int,write_err)result m;
    read: file_descr -> bytes -> int -> int -> (int,read_err)result m;
  }
end


module Make_msg_lib(Net_ops:NET_OPS) = struct
  open Net_ops
  let ( >>= ) = bind

  (* NOTE in the following, lower level errors are returned as Error
     e; errors at this level are returned as Ok ... *)
  let mk_msg_lib ~(ops:net_ops) = 

    (* accept connections for this quad only *)
    let listen_accept ~quad = 
      return () >>= fun () ->
      ops.socket Unix.PF_INET Unix.SOCK_STREAM 0 >>= function | Error e -> return (Error `Socket) | Ok srvr ->
        begin
          (* hack to speed up recovery *)
          ops.setsockopt srvr Unix.SO_REUSEADDR true >>= fun () ->
          ops.bind_ srvr quad.local >>= function | Error e -> return (Error `Bind) | Ok () -> 
            ops.listen srvr 5 >>= function | Error e -> return (Error `Other) | Ok () ->
              ops.accept srvr >>= function | Error e -> return (Error `Other) | Ok (c,_) ->              
                ops.getpeername c >>= fun pn -> 
                if pn <> quad.remote then 
                  (* connection doesn't match quad *)                
                  ops.close c >>= fun () ->
                  return (Error `Peername)
                else
                  return (Ok c)
        end >>= begin fun r -> 
          ops.close srvr >>= fun () ->
          return r
        end
    in

    let _ = listen_accept in  

    let connect ~quad = 
      return () >>= fun () ->
      ops.socket Unix.PF_INET Unix.SOCK_STREAM 0 >>= function | Error e -> return (Error `Socket) | Ok c ->
        begin
          (* hack to speed up recovery *)
          ops.setsockopt c Unix.SO_REUSEADDR true >>= fun () ->
          ops.bind_ c quad.local >>= function Error e -> return (Error `Bind) | Ok () ->
            ops.connect c quad.remote >>= function Error e -> return (Error `Connect) | Ok () ->
              return (Ok c)
        end >>= function
        | Error e -> ops.close c >>= fun () -> return (Error e)
        | Ok c -> return (Ok c)
    in
    
    let _ = connect in
    
    (* send, recv ------------------------------------------------------- *)

    (* send length as 4 bytes, then the string itself; NOTE for
       performance, it is quite important to try to call write with a
       buffer which includes everything to do with the message *)
    let send_string ~conn s =
      return () >>= fun () ->
      String.length s |> fun len ->
      let buf = Bytes.create (4+len) in
      i2bs ~buf ~off:0 ~i:len ~n:4;
      Bytes.blit_string s 0 buf 4 len;
      (* now write the buffer *)
      ops.write conn buf 0 (4+len) >>= function Error e -> return (Error()) | Ok nwritten ->
      match (nwritten=4+len) with
      | true -> return (Ok())
      | false -> return (Error ())
    in

    let _ = send_string in

    (* send nstrings, followed by strings *)
    let send_strings ~conn (strings:string list) =
      return () >>= fun () ->
      Marshal.to_string strings [] |> fun s ->
      send_string ~conn s
    in

    let _ = send_strings in

    (* actually read len bytes *)
    let rec read_n ~conn ~buf ~off ~len = 
      return () >>= fun () ->
      len |> function
      | 0 -> return (Ok())
      | _ -> 
        ops.read conn buf off len >>= function Error e -> return (Error()) | Ok nread ->
        (* FIXME when connection closed, this should return error in monad *)
        match nread with 
        | 0 -> return (Error())
        | _ -> read_n ~conn ~buf ~off:(off+nread) ~len:(len-nread)
    in

    let read_length ~conn =
      return () >>= fun () ->
      Bytes.create 4 |> fun buf ->
      read_n ~conn ~buf ~off:0 ~len:4 >>= function Error e -> return (Error()) | Ok () ->
        bs2i ~buf ~off:3 ~n:4 |> fun i ->
        return (Ok i)
    in

    let recv_string ~conn =
      return () >>= fun () ->
      read_length ~conn >>= function Error e -> return (Error()) | Ok len ->
        Bytes.create len |> fun buf ->          
        read_n ~conn ~buf ~off:0 ~len >>= function Error e -> return (Error()) | Ok () ->
          Bytes.unsafe_to_string buf |> fun s -> return (Ok s)
    in


    (* FIXME marshal is a bit platform-specific; send # strings, then
       each string *)
    let recv_strings ~conn = 
      return () >>= fun () ->
      recv_string ~conn >>= function Error e -> return (Error()) | Ok s ->
        Marshal.from_string s 0 |> fun (ss:string list) ->
        return (Ok ss)
    in

    fun k -> k ~listen_accept ~connect ~send_string ~send_strings ~recv_string ~recv_strings

  let _ = mk_msg_lib

end


module Unix_ = struct
  open Unix

  module File_descr = struct
    type file_descr = Unix.file_descr
  end
  include File_descr

  type unix_error = Unix.error * string * string

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

  module Net_ops_type = Make_net_ops_type(File_descr)(Monad)
  include Net_ops_type

  module Msg_lib = Make_msg_lib(
    struct
      include File_descr
      include Monad
      include Net_ops_type
    end)
  include Msg_lib


  (* NOTE this traps exceptions from the lower level and passes them into the monad *)
  let wrap f = 
    try Ok(f ()) with 
    | Unix.Unix_error (e,s1,s2) -> Error `EOTHER
  (* TODO refine this to match the errors eg socket_err *)
  (* NOTE other exceptions are not caught *)

  let wrap1 s f = fun a -> wrap @@ fun () -> log_ s; f a
  let wrap2 s f = fun a b -> wrap @@ fun () -> log_ s; f a b 
  let wrap3 s f = fun a b c -> wrap @@ fun () -> log_ s; f a b c 
  let wrap4 s f = fun a b c d -> wrap @@ fun () -> log_ s; f a b c d

  let ops = {
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

  let (listen_accept,connect,send_string,send_strings,recv_string,recv_strings) = 
    mk_msg_lib ~ops @@ 
    fun ~listen_accept ~connect ~send_string ~send_strings ~recv_string ~recv_strings -> 
    (listen_accept,connect,send_string,send_strings,recv_string,recv_strings)

  let _ = listen_accept

end


module Lwt_ = struct
  open Lwt
  open Lwt_unix
  module File_descr = struct
    type file_descr = Lwt_unix.file_descr
  end
  include File_descr

  module Monad = struct
    type 'a m = 'a Lwt.t
    let return,bind = Lwt.(return,bind)
  end 
  include Monad

  module Net_ops_type = Make_net_ops_type(File_descr)(Monad)
  include Net_ops_type

  module Msg_lib = Make_msg_lib(
    struct
      include File_descr
      include Monad
      include Net_ops_type
    end)
  include Msg_lib

  (* TODO refine errors *)
  let wrap f = catch (fun () -> f () >>= fun x -> return (Ok x)) (fun e -> return (Error `EOTHER))

  let ops = {
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

  let (listen_accept,connect,send_string,send_strings,recv_string,recv_strings) = 
    mk_msg_lib ~ops @@ 
    fun ~listen_accept ~connect ~send_string ~send_strings ~recv_string ~recv_strings -> 
    (listen_accept,connect,send_string,send_strings,recv_string,recv_strings)

  let _ = listen_accept
  let _ = connect 
end