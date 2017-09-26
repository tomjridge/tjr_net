(* network connections *)


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
  assert_ (Bytes.length buf >= 4+off);
  let rec f ~off ~i ~n = n |> function
    | 0 -> i
    | _ -> 
      Bytes.get buf off |> Char.code |> fun b ->
      co_strip_byte b i |> fun i ->
      f ~off:(off-1) ~i ~n:(n-1)
  in
  f ~off ~i:0 ~n

open Lwt

open Lwt_unix

type conn = file_descr
type ip = inet_addr
type port = int
type ipp = sockaddr (*  expect ADDR_INET ip * port *)
type quad = { local:ipp; remote: ipp }



(* accept connections for this quad only *)
let listen_accept ~quad = 
  socket PF_INET SOCK_STREAM 0 |> fun (srvr:file_descr) ->
  let listen = fun () ->
    (* hack to speed up recovery *)
    setsockopt srvr SO_REUSEADDR true;
    let addr = quad.local in
    bind srvr addr >>= fun () ->
    listen srvr 5;
    accept srvr >>= fun (c,_) ->
    if getpeername c <> quad.remote then 
      close c >>= fun () -> fail_with __LOC__  (* connection doesn't match quad *)
    else 
      return c
  in
  finalize listen (fun () -> close srvr)


let connect ~quad = 
  socket Unix.PF_INET Unix.SOCK_STREAM 0 |> fun (c:file_descr) ->
  let connect () = 
    (* hack to speed up recovery *)
    setsockopt c SO_REUSEADDR true;
    bind c quad.local >>= fun () ->
    connect c quad.remote >>= fun () ->
    return c
  in
  finalize connect (fun () -> close c)


(* send, recv ------------------------------------------------------- *)

(* actually read len bytes *)
let rec read_n ~conn ~buf ~off ~len = 
  len |> function
  | 0 -> return_unit
  | _ -> 
    read conn buf off len >>= fun nread ->
    read_n ~conn ~buf ~off:(off+nread) ~len:(len-nread)
  

let read_length ~conn : int t = 
  Bytes.create 4 |> fun buf ->
  read_n ~conn ~buf ~off:0 ~len:4 >>= fun () ->
  bs2i ~buf ~off:0 |> fun i ->
  assert (p.mark' P.hi);
  i)
