(* this is a version based on representing the calls concretely and
   interpreting them in some particular monad *)

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

module type FILE_DESCR = sig
  type file_descr
end

module type MONAD = sig
  type 'a m
  val return : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
end 

module type NET_OPS = sig
  open Unix
  include FILE_DESCR
  include MONAD
  type 'e net_ops = {
    socket: socket_domain -> socket_type -> int -> (file_descr,'e) result m;
    setsockopt: file_descr -> socket_bool_option -> bool -> (unit,'e) result m;
    bind_: file_descr -> sockaddr -> (unit,'e) result m;
    listen: file_descr -> int -> (unit,'e) result m;
    accept: file_descr -> (file_descr * sockaddr,'e) result m;
    getpeername: file_descr -> (sockaddr,'e) result m;
    close: file_descr -> unit m;  (* NOTE does not return result *)
    connect: file_descr -> sockaddr -> (unit,'e) result m;
    write: file_descr -> bytes -> int -> int -> (int,'e) result m;
    read: file_descr -> bytes -> int -> int -> (int,'e) result m;
  }
end

module Make_net_ops_type(FD:FILE_DESCR)(M:MONAD) = struct
  open Unix
  open FD
  open M
  type 'e net_ops = {
    socket: socket_domain -> socket_type -> int -> (file_descr,'e) result m;
    setsockopt: file_descr -> socket_bool_option -> bool -> (unit,'e) result m;
    bind_: file_descr -> sockaddr -> (unit,'e) result m;
    listen: file_descr -> int -> (unit,'e) result m;
    accept: file_descr -> (file_descr * sockaddr,'e) result m;
    getpeername: file_descr -> (sockaddr,'e) result m;
    close: file_descr -> unit m;  (* NOTE does not return a result? *)
    connect: file_descr -> sockaddr -> (unit,'e) result m;
    write: file_descr -> bytes -> int -> int -> (int,'e) result m;
    read: file_descr -> bytes -> int -> int -> (int,'e) result m;
  }
end


module Make_msg_lib(Net_ops:NET_OPS) = struct
  open Net_ops
  let ( >>= ) = bind
  let ( >>== ) = bind (* save *)

  (* we want to use a bind that stops when an Error e is returned *)
  let ( >>= ) a b = a >>= function
    | Ok a -> b a
    | Error e -> return (Error e)

  (* NOTE that this is parameterized by the actual errors that might
     be involved, so different implementations can catch different
     errors; but the type of errors is fixed over all functions *)
  type 'e extra = {
    catch: 'a. ('e -> 'a m) -> 'a m -> 'a m;
  }

  (* these are pulled out from the code below FIXME don't we want to distinguish between lower level errors and errors at our level? and isn't this what the type of `Net_err was telling us? *)
  type e = [ 
      `Listen_accept_incorrect_peername | 
      `Net_err of e | 
      `Read_n_read_error | 
      `Send_string_write_error ]

  let mk_msg_lib ~(ops:e net_ops) ~(catch:e extra) = 
    (* accept connections for this quad only *)
    let listen_accept ~quad = 
      ops.socket Unix.PF_INET Unix.SOCK_STREAM 0 >>= fun srvr ->
      begin
        (* hack to speed up recovery *)
        ops.setsockopt srvr Unix.SO_REUSEADDR true >>= fun () ->
        ops.bind_ srvr quad.local >>= fun () -> 
        ops.listen srvr 5 >>= fun () ->
        ops.accept srvr >>= fun (c,_) ->
        ops.getpeername c >>= fun pn -> 
        if pn <> quad.remote then 
          (* connection doesn't match quad *)
          ops.close c >>== fun () -> return @@ Error `Listen_accept_incorrect_peername
        else
          return @@ Ok c
      end
      |> catch.catch (function 
          (* NOTE this is an error from the lower level *)
          | e -> ops.close srvr >>== fun () -> return @@ Error (`Net_err e))
    in

    let _ = listen_accept in  
    (* FIXME would be nice to know which errors each function could
       throw... include this in monad type? *)


    let connect ~quad = 
      ops.socket Unix.PF_INET Unix.SOCK_STREAM 0 >>= fun c ->
      begin
        (* hack to speed up recovery *)
        ops.setsockopt c Unix.SO_REUSEADDR true >>= fun () ->
        ops.bind_ c quad.local >>= fun () ->
        ops.connect c quad.remote >>= fun () ->
        return @@ Ok c
      end
      |> catch.catch (function
          | e -> ops.close c >>== fun () -> return @@ Error (`Net_err e))
    in


    (* send, recv ------------------------------------------------------- *)

    (* send length as 4 bytes, then the string itself; NOTE for
       performance, it is quite important to try to call write with a
       buffer which includes everything to do with the message *)
    let send_string ~conn s =
      return (Ok ()) >>= fun () ->
      String.length s |> fun len ->
      let buf = Bytes.create (4+len) in
      i2bs ~buf ~off:0 ~i:len ~n:4;
      Bytes.blit_string s 0 buf 4 len;
      (* now write the buffer *)
      ops.write conn buf 0 (4+len) >>= fun nwritten ->
      match (nwritten=4+len) with
      | true -> return (Ok ())
      | false -> return (Error `Send_string_write_error)
    in

    let _ = send_string in

    (* send nstrings, followed by strings *)
    let send_strings ~conn (strings:string list) =
      return (Ok ()) >>= fun () ->
      Marshal.to_string strings [] |> fun s ->
      send_string ~conn s
    in

    let _ = send_strings in

    (* actually read len bytes *)
    let rec read_n ~conn ~buf ~off ~len = 
      return (Ok ()) >>= fun () ->
      len |> function
      | 0 -> return (Ok ())
      | _ -> 
        ops.read conn buf off len >>= fun nread ->
        (* FIXME when connection closed, this should return error in monad *)
        match nread with 
        | 0 -> return (Error `Read_n_read_error)
        | _ -> read_n ~conn ~buf ~off:(off+nread) ~len:(len-nread)
    in

    let read_length ~conn : (int,'e) result m =
      return (Ok ()) >>= fun () ->
      Bytes.create 4 |> fun buf ->
      read_n ~conn ~buf ~off:0 ~len:4 >>= fun () ->
      bs2i ~buf ~off:3 ~n:4 |> fun i ->
      return (Ok i)
    in

    let recv_string ~conn : (string,'e) result m =
      return (Ok ()) >>= fun () ->
      read_length ~conn >>= fun len -> 
      Bytes.create len |> fun buf ->          
      read_n ~conn ~buf ~off:0 ~len >>= fun () ->
      Bytes.unsafe_to_string buf |> fun s -> return (Ok s)
    in


    (* FIXME marshal is a bit platform-specific? *)
    let recv_strings ~conn : (string list,'e) result m = 
      return (Ok ()) >>= fun () ->
      recv_string ~conn >>= fun s ->
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

  module Monad = struct
    (* NOTE the question is how to propagate errors from the lower
       level; ideally this should happen invisibly, hence the exn in the
       following; however, catch should function with these errors *)
    type 'a m = ('a,exn)result
    let return x = Ok x
    let bind a b = match a with |Ok a -> b a | Error e -> Error e
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
  let wrap f = try Ok(f ()) with e -> Error e

  let wrap1 f = fun a -> wrap @@ fun () -> f a
  let wrap2 f = fun a b -> wrap @@ fun () -> f a b 
  let wrap3 f = fun a b c -> wrap @@ fun () -> f a b c 
  let wrap4 f = fun a b c d -> wrap @@ fun () -> f a b c d

  let ops = {
    socket=(wrap3 socket);
    setsockopt=(wrap3 setsockopt);
    bind_=(wrap2 Unix.bind);
    listen=(wrap2 listen);
    accept=(wrap1 accept);
    getpeername=(wrap1 getpeername);
    close=(wrap1 close);
    connect=(wrap2 connect);
    write=(wrap4 write);
    read=(wrap4 read);
  }

  let catch = {
    catch=fun f a -> 
      match a with
      | Ok x -> Ok x
      | Error e -> f e
  }

  let (listen_accept,connect,send_string,send_strings,recv_string,recv_strings) = 
    mk_msg_lib ~ops ~catch @@ 
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

  let ops = {
    socket=(fun a b c -> socket a b c |>return);
    setsockopt=(fun a b c -> setsockopt a b c |> return);
    bind_=Lwt_unix.bind;
    listen=(fun a b -> listen a b|>return);
    accept;
    getpeername=(fun x -> getpeername x |>return);
    close;
    connect;
    write;
    read;
  }

  let catch = {
    catch=fun f a -> Lwt.catch (fun () -> a) f
  }
  

  let (listen_accept,connect,send_string,send_strings,recv_string,recv_strings) = 
    mk_msg_lib ~ops ~catch @@ 
    fun ~listen_accept ~connect ~send_string ~send_strings ~recv_string ~recv_strings -> 
    (listen_accept,connect,send_string,send_strings,recv_string,recv_strings)

  let _ = listen_accept


end
