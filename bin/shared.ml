(* NOTE we specialize to Lwt instance *)

open Lwt_unix

module Config = struct
  type config = { ip:string; rport:int; sport: int } [@@deriving yojson]

  let default_config = Some {
      ip = "127.0.0.1";
      rport=4001;
      sport=4007;
    }

  let filename = "tjr_net.config"
end

module Config' = Tjr_config.Make(Config)

let config = Config'.config

let ip = Unix.inet_addr_of_string config.ip
let s = ADDR_INET(ip,config.sport)
let r = ADDR_INET(ip,config.rport)

let sender = { local=s; remote=r }
let recvr = {local=r; remote=s }

let _ = Sys.signal Sys.sigpipe Sys.Signal_ignore

(* FIXME is this needed? if receiver is not present we get Fatal
   error: exception Sys_error("Bad file descriptor") sometimes *)
(*
let _ = Sys.signal Sys.sigabrt Sys.Signal_ignore
let _ = Sys.signal Sys.sighup Sys.Signal_ignore
let _ = Sys.signal Sys.sigchld Sys.Signal_ignore
let _ = Sys.signal Sys.sigttin Sys.Signal_ignore
*)
