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



(* messaging -------------------------------------------------------- *)

include struct
open Unix (* for PF_NET SOCK_STREAM etc *)

(* Lwt_unix has file_descr <> Unix.file_descr *)

(* type 'a conn = File_descr of 'a *)
type ip = inet_addr
type port = int
type ipp = sockaddr (*  expect ADDR_INET ip * port *)
type quad = { local:ipp; remote: ipp }
end


(* end aux ---------------------------------------------------------- *)


type file_descr = Unix.file_descr  (* for lwt, maintain a bijection *)

module Ops = struct

  (* FIXME may want calls such as socket, bind, listen, connect to return an explicit error; keep implciit monad error for read and write *)
  type 'a call =
    | Socket: Unix.socket_domain * Unix.socket_type * int -> file_descr call
    | Setsockopt: file_descr * Unix.socket_bool_option * bool -> unit call
    | Bind: file_descr * Unix.sockaddr -> unit call
    | Listen: file_descr * int -> unit call
    | Accept: file_descr -> (file_descr * Unix.sockaddr) call
    | Getpeername: file_descr -> Unix.sockaddr call
    | Close: file_descr -> unit call
    | Connect: file_descr * Unix.sockaddr -> unit call
    | Write: file_descr * bytes * int * int -> int call
    | Read: file_descr * bytes * int * int -> int call


  type 'a m = 
    | Return of 'a 
    | Call of 'a call
    | MBind: 'a m * ('a -> 'b m) -> 'b m
    | Catch: (exn -> 'a m)*'a m -> 'a m


  let call x = Call x
  let return x = Return x

  let socket dom ty prot = Socket(dom,ty,prot)|>call
  let _ = socket
  let setsockopt fd n b = Setsockopt(fd,n,b)|>call
  let bind fd addr = Bind(fd,addr)|>call
  let listen fd i = Listen(fd,i)|>call
  let accept fd = Accept(fd)|>call
  let getpeername fd = Getpeername fd|>call
  let close fd = Close fd|>call
  let connect fd addr = Connect(fd,addr)|>call
  let write fd bs i j = Write(fd,bs,i,j)|>call
  let read fd bs i j = Read(fd,bs,i,j)|>call

end




(* what we need from the lower level *)

let mk_msg_lib () = 
  let open Ops in
  let catch x y = Catch(x,y) in
  let ( >>= ) x y = MBind(x,y) in
  
  (* accept connections for this quad only *)
  let listen_accept ~quad = 
    socket Unix.PF_INET Unix.SOCK_STREAM 0 >>= fun srvr ->
    begin
      (* hack to speed up recovery *)
      setsockopt srvr Unix.SO_REUSEADDR true >>= fun () ->
      bind srvr quad.local >>= fun () -> 
      listen srvr 5 >>= fun () ->
      accept srvr >>= fun (c,_) ->
      getpeername c >>= fun pn -> 
      if pn <> quad.remote then 
        (* connection doesn't match quad *)
        close c >>= fun () -> return `Error_incorrect_peername
      else
        return @@ `Connection c
    end
    |> catch (function 
      (* NOTE this is an error from the lower level *)
      | e -> close srvr >>= fun () -> return @@ `Net_err e)
  in

  let _ = listen_accept in  
  (* FIXME would be nice to know which errors each function could
     throw... include this in monad type? *)


  let connect ~quad = 
    socket Unix.PF_INET Unix.SOCK_STREAM 0 >>= fun c ->
    begin
      (* hack to speed up recovery *)
      setsockopt c Unix.SO_REUSEADDR true >>= fun () ->
      bind c quad.local >>= fun () ->
      connect c quad.remote >>= fun () ->
      return @@ `Connection c
    end
    |> catch (function
      | e -> close c >>= fun () -> return @@ `Net_err e)
  in


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
    write conn buf 0 (4+len) >>= fun nwritten ->
    assert_(nwritten=4+len);  (* FIXME or loop? *)
    return ()
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
    | 0 -> return ()
    | _ -> 
      read conn buf off len >>= fun nread ->
      read_n ~conn ~buf ~off:(off+nread) ~len:(len-nread)
  in

  let read_length ~conn : int m =
    return () >>= fun () ->
    Bytes.create 4 |> fun buf ->
    read_n ~conn ~buf ~off:0 ~len:4 >>= fun () ->
    bs2i ~buf ~off:3 ~n:4 |> fun i ->
    return i
  in

  let recv_string ~conn : string m =
    return () >>= fun () ->
    read_length ~conn >>= fun len -> 
    Bytes.create len |> fun buf ->          
    read_n ~conn ~buf ~off:0 ~len >>= fun () ->
    Bytes.unsafe_to_string buf |> return
  in


  (* FIXME marshal is a bit platform-specific? *)
  let recv_strings ~conn : string list m = 
    return () >>= fun () ->
    recv_string ~conn >>= fun s ->
    Marshal.from_string s 0 |> fun (ss:string list) ->
    return ss
  in

  fun k -> k ~listen_accept ~connect ~send_string ~send_strings ~recv_string ~recv_strings


let _ = mk_msg_lib




module Unix_ = struct
  open Ops

  let interp_call =
    let f : type a. a call -> a = fun x ->
      match x with
      | Socket(dom,ty,prot) -> Unix.socket dom ty prot
      | Setsockopt(fd,n,b) -> Unix.setsockopt fd n b
      | Bind(fd,addr) -> Unix.bind fd addr
      | Listen(fd,i) -> Unix.listen fd i
      | Accept fd -> Unix.accept fd
      | Getpeername fd -> Unix.getpeername fd
      | Close fd -> Unix.close fd
      | Connect (fd,addr) -> Unix.connect fd addr
      | Write(fd,bs,i,j) -> Unix.write fd bs i j
      | Read(fd,bs,i,j) -> Unix.read fd bs i j
    in
    f

  let interp_call c = try Ok (interp_call c) with e -> Error e 

  let rec interp: type a. a Ops.m -> (a,exn) result = function
    | Return x -> Ok x
    | Call c -> interp_call c
    | MBind(a,b) -> (interp a |> function
      | Ok a -> interp (b a)
      | Error e -> Error e)
    | Catch(f,a) -> 
      interp a |> function
      | Ok a -> Ok a
      | Error e -> interp (f e)


  let mk_msg_lib () = 
    mk_msg_lib () @@ fun ~listen_accept ~connect ~send_string ~send_strings ~recv_string ~recv_strings -> 

    let listen_accept ~quad = listen_accept ~quad |> interp in
    let connect ~quad = connect ~quad |> interp in
    let send_string ~conn s = send_string ~conn s |> interp in
    let send_strings ~conn ss = send_strings ~conn ss |> interp in
    let recv_string ~conn = recv_string ~conn |> interp in
    let recv_strings ~conn = recv_strings ~conn |> interp in
    fun k -> k ~listen_accept ~connect ~send_string ~send_strings ~recv_string ~recv_strings

end


module Lwt_ = struct
  open Ops
  module L = Lwt_unix

  let return = Lwt.return

  (* for lwt, the type of file descriptor is different from Unix; in
     particular, it has extra fields recording various types of state; so
     as a quick hack we record Unix.fd -> lwt.fd map *)

  let tbl = Hashtbl.create 100

  let add (lwt_fd:Lwt_unix.file_descr) = 
    L.unix_file_descr lwt_fd |> fun fd ->
    Hashtbl.add tbl fd lwt_fd;
    ()


  let get fd = 
    try Hashtbl.find tbl fd
    with Not_found -> assert false


  let clear fd =
    Hashtbl.remove tbl fd


  let fd2unix' lwt_fd =
    add lwt_fd;
    L.unix_file_descr lwt_fd

  let fd2lwt fd = 
    get fd

  let fd2unix lwt_fd = fd2unix' lwt_fd |> return

  let ( >>= ) = Lwt.bind

  let interp_call = 
    let f : type a. a call -> a Lwt.t = fun x ->
      match x with
      | Socket(dom,ty,prot) -> L.socket dom ty prot |> fd2unix
      | Setsockopt(fd,n,b) -> L.setsockopt (fd2lwt fd) n b |> return
      | Bind(fd,addr) -> L.bind (fd2lwt fd) addr
      | Listen(fd,i) -> L.listen (fd2lwt fd) i |> return
      | Accept fd -> 
        L.accept (fd2lwt fd) >>= fun (fd,addr) -> 
        return (fd2unix' fd,addr)
      | Getpeername fd -> L.getpeername (fd2lwt fd) |> return
      | Close fd -> 
        (* assume that no other thread is using fd *)
        clear fd; L.close (fd2lwt fd)
      | Connect (fd,addr) -> L.connect (fd2lwt fd) addr
      | Write(fd,bs,i,j) -> L.write (fd2lwt fd) bs i j
      | Read(fd,bs,i,j) -> L.read (fd2lwt fd) bs i j
    in
    f


  let rec interp: type a. a Ops.m -> a Lwt.t = function
    | Return x -> return x
    | Call c -> interp_call c
    | MBind(a,b) -> (interp a >>= fun a -> interp (b a))
    | Catch(f,a) -> 
      Lwt.catch 
        (fun () -> interp a) 
        (fun e -> interp (f e))


  (* copied verbatim from unix *)
  let mk_msg_lib () = 
    mk_msg_lib () @@ fun ~listen_accept ~connect ~send_string ~send_strings ~recv_string ~recv_strings -> 

    let listen_accept ~quad = listen_accept ~quad |> interp in
    let connect ~quad = connect ~quad |> interp in
    let send_string ~conn s = send_string ~conn s |> interp in
    let send_strings ~conn ss = send_strings ~conn ss in
    let recv_string ~conn = recv_string ~conn |> interp in
    let recv_strings ~conn = recv_strings ~conn |> interp in
    fun k -> k ~listen_accept ~connect ~send_string ~send_strings ~recv_string ~recv_strings


end
