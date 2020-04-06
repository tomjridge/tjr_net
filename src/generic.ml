(** Basic networking utils; generic code *)

open Net_intf

(* FIXME tidy this up and release; split unix to separate package *)

let log_ s = ()

module Pvt_util = struct


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
end
open Pvt_util

(** Construct the message queue, given some underlying networking operations *)
module Make(S: sig
    include MONAD
    type fd
  end) =  struct
  open S
  let ( >>= ) = bind

  include Net_intf.Make_net_ops(S)

  include Net_intf.Make_msg_ops(S)

  (* NOTE in the following, lower level errors are returned as Error
     e; errors at this level are returned as Ok ... *)
  let mk_msg_lib ~(ops:net_ops) : msg_ops =

    let err e = return (Error e) in
    let ok x = return (Ok x) in

    (* accept connections for this quad only *)
    let listen_accept quad =
      return () >>= fun () ->
      ops.socket Unix.PF_INET Unix.SOCK_STREAM 0 >>= function
      | Error _e -> err `Socket
      | Ok srvr ->
        begin
          (* hack to speed up recovery *)
          ops.setsockopt srvr Unix.SO_REUSEADDR true >>= fun () ->
          ops.bind_ srvr quad.local >>= function | Error _e -> err `Bind | Ok () ->
            ops.listen srvr 5 >>= function | Error _e -> err `Other | Ok () ->
              ops.accept srvr >>= function | Error _e -> err `Other | Ok (c,_) ->
                ops.getpeername c >>= fun pn ->
                if pn <> quad.remote then
                  (* connection doesn't match quad *)
                  ops.close c >>= fun () ->
                  err `Peername
                else
                  ok c
        end >>= begin fun r ->
          ops.close srvr >>= fun () ->
          return r
        end
    in

    let _ = listen_accept in

    let connect quad =
      return () >>= fun () ->
      ops.socket Unix.PF_INET Unix.SOCK_STREAM 0 >>= function
      | Error _e -> err `Socket
      | Ok c ->
        begin
          (* hack to speed up recovery *)
          ops.setsockopt c Unix.SO_REUSEADDR true >>= fun () ->
          ops.bind_ c quad.local >>= function Error _e -> err `Bind | Ok () ->
            ops.connect c quad.remote >>= function Error _e -> err `Connect | Ok () ->
              ok c
        end >>= function
        | Error e -> ops.close c >>= fun () -> err e
        | Ok c -> ok c
    in

    let _ = connect in

    (* send, recv ------------------------------------------------------- *)

    (* send length as 4 bytes, then the string itself; NOTE for
       performance, it is quite important to try to call write with a
       buffer which includes everything to do with the message; NOTE
       OCaml strings are fixed-length sequence of single-byte chars,
       so we don't need to worry about unicode (?) *)
    let send_string conn s =
      return () >>= fun () ->
      String.length s |> fun len ->
      let buf = Bytes.create (4+len) in
      i2bs ~buf ~off:0 ~i:len ~n:4;
      Bytes.blit_string s 0 buf 4 len;
      (* now write the buffer *)
      ops.write conn buf 0 (4+len) >>= function Error _e -> err () | Ok nwritten ->
      match (nwritten=4+len) with
      | true -> ok ()
      | false -> err ()
    in

    let _ = send_string in

    (* send nstrings, followed by strings *)
    let send_strings conn (strings:string list) =
      return () >>= fun () ->
      Marshal.to_string strings [] |> fun s ->
      send_string conn s
    in

    let _ = send_strings in

    (* actually read len bytes *)
    let rec read_n ~conn ~buf ~off ~len =
      return () >>= fun () ->
      len |> function
      | 0 -> ok ()
      | _ ->
        ops.read conn buf off len >>= function Error _e -> err () | Ok nread ->
        (* FIXME when connection closed, this should return error in monad *)
        match nread with
        | 0 -> err ()
        | _ -> read_n ~conn ~buf ~off:(off+nread) ~len:(len-nread)
    in

    let read_length ~conn =
      return () >>= fun () ->
      Bytes.create 4 |> fun buf ->
      read_n ~conn ~buf ~off:0 ~len:4 >>= function Error _e -> return (Error()) | Ok () ->
        bs2i ~buf ~off:3 ~n:4 |> fun i ->
        ok i
    in

    let recv_string conn =
      return () >>= fun () ->
      read_length ~conn >>= function Error _e -> err () | Ok len ->
        Bytes.create len |> fun buf ->
        read_n ~conn ~buf ~off:0 ~len >>= function Error _e -> err () | Ok () ->
          Bytes.unsafe_to_string buf |> fun s -> ok s
    in


    (* FIXME marshal is a bit platform-specific; send # strings, then
       each string *)
    let recv_strings conn =
      return () >>= fun () ->
      recv_string conn >>= function Error _e -> err () | Ok s ->
        Marshal.from_string s 0 |> fun (ss:string list) ->
        ok ss
    in

    { listen_accept; connect; send_string; send_strings; recv_string; recv_strings }

  let _ = mk_msg_lib

end
