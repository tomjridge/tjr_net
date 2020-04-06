(** Basic network types: addresses, errors etc

The {!Generic} code consumes a set of network operations, and produces
   a set of message lib operations. This code was really an experiment
   to try to abstract over Stdlib.Unix (no monad) and Lwt_unix.
 *)

open Unix (* for PF_NET SOCK_STREAM etc *)

(* NOTE Lwt_unix has file_descr <> Unix.file_descr *)



(** {2 Address types } *)

(* type ip = inet_addr *)
type inet_addr = Unix.inet_addr

type port = int

(* was ipp *)
type endpt = sockaddr (** expect ADDR_INET ip * port *)

type endpt_pair = { local:endpt; remote:endpt }
(** A pair of endpoints, local and remote *)


(** {2 Net error types } *)


module Net_err = struct
  (* NOTE these types identify some of the errors; to be refined
     later as needed *)
  type socket_err  = [ `ERESOURCE | `EOTHER ]

  type bind_err    = [ `EADDRINUSE | `EOTHER ]

  type listen_err  = [ `EOTHER ]

  type accept_err  = [ `EOTHER ]

  type connect_err = [ `EREFUSED | `EHOSTUNREACH | `EOTHER ]

  type read_err    = [ `EOTHER ]

  type write_err   = [ `EOTHER ]
end
open Net_err

(** {2 Underlying network operations} *)

(* NOTE bind already used by monad *)

type ('a,'b) r_ = ('a,'b) result

module Make_net_ops(S: sig type 'a m type fd end) = struct
  open S

  (** The main operations we need, abstracted from Lwt_unix and Unix *)
  type net_ops = {
    socket      : socket_domain -> socket_type -> int -> (fd,socket_err)r_ m;
    setsockopt  : fd -> socket_bool_option -> bool ->    unit m;
    bind_       : fd -> sockaddr ->                      (unit,bind_err)r_ m;
    listen      : fd -> int ->                           (unit,listen_err)r_ m;
    accept      : fd ->  (fd * sockaddr,accept_err)r_ m;
    getpeername : fd ->  sockaddr m;  (* option ?*)
    close       : fd ->  unit m;  
    connect     : fd -> sockaddr ->                      (unit,connect_err)r_ m;
    write       : fd -> bytes -> int -> int ->           (int,write_err)r_ m;
    read        : fd -> bytes -> int -> int ->           (int,read_err)r_ m;
  }
end


(** {2 Msg error types} *)

module Msg_err = struct
  type listen_accept_err = [`Bind | `Other | `Peername | `Socket ]
  type connect_err       = [`Bind | `Connect | `Socket]
end
open Msg_err


(** {2 Msg-lib operations} *)

module Make_msg_ops(S: sig type 'a m type fd end) = struct
  open S
  type msg_ops = {
    listen_accept : endpt_pair -> (fd,listen_accept_err)r_ m;
    connect       : endpt_pair -> (fd,connect_err)r_ m;
    send_string   : fd -> string -> (unit,unit) r_ m;
    send_strings  : fd -> string list -> (unit,unit) r_ m;
    recv_string   : fd -> (string,unit) r_ m;
    recv_strings  : fd -> (string list,unit) r_ m;    
  }
end



(** {2 Monad sig} *)

(** Monad intf *)
module type MONAD = sig
  type 'a m
  val return : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
end 

(* module type FD = sig type fd end *)

(*
(** "Full" interface, with components all combined in a single sig *)
module type NET_OPS = sig
  include FILE_DESCR
  include MONAD

  (* NOTE errors from these lower-level functions are captured in the
     monad; but they can be dealt with at the higher level by using
     catch and finally *)

  type fd = file_descr

  (* NOTE bind already used by monad *)

  type net_ops = {
    socket      : socket_domain -> socket_type -> int -> (fd,socket_err)result m;
    setsockopt  : fd -> socket_bool_option -> bool -> unit m;
    bind_       : fd -> sockaddr -> (unit,bind_err)result m;
    listen      : fd -> int -> (unit,listen_err)result m;
    accept      : fd -> (fd * sockaddr,accept_err)result m;
    getpeername : fd -> sockaddr m;  (* option ?*)
    close       : fd -> unit m;  
    connect     : fd -> sockaddr -> (unit,connect_err)result m;
    write       : fd -> bytes -> int -> int -> (int,write_err)result m;
    read        : fd -> bytes -> int -> int -> (int,read_err)result m;
  }

end

(** Construct the net_ops type, given some underlying fd and monad types *)
module Make_net_ops_type(FD:FILE_DESCR)(M:MONAD) = struct
  open Unix
  open FD
  open M

  type fd = file_descr

  type net_ops = {
    socket      : socket_domain -> socket_type -> int -> (file_descr,socket_err)result m;
    setsockopt  : file_descr -> socket_bool_option -> bool -> unit m;
    bind_       : file_descr -> sockaddr -> (unit,bind_err)result m;
    listen      : file_descr -> int -> (unit,listen_err)result m;
    accept      : file_descr -> (file_descr * sockaddr,accept_err)result m;
    getpeername : file_descr -> sockaddr m;  (* option ?*)
    close       : file_descr -> unit m;  
    connect     : file_descr -> sockaddr -> (unit,connect_err)result m;
    write       : file_descr -> bytes -> int -> int -> (int,write_err)result m;
    read        : file_descr -> bytes -> int -> int -> (int,read_err)result m;
  }
end

*)
