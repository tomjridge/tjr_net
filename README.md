# Basic networking utilities; very rough; do not use!

This repository contains some very simple networking code. 



## Build

To build, type `make`



## Before running executables

In order to run the examples, you first need to copy `bin/tjr_net.config` to `/tmp`



## Running executables

There are some simple examples in the bin/ directory: receiver.ml and sender.ml

Run each of these at the same time, and some messages should be exchanged.

To run, execute: `make run_receiver` (this needs to come first) and `make run_sender`



## OCamldoc

See <https://tomjridge.github.io/ocamldocs/>



## Dependencies



| Dependency | Description                                       |
| ---------- | ------------------------------------------------- |
| lwt        |                                                   |
| lwt.unix?  | Maybe installed with lwt?                         |
| tjr_lib    | For executable examples (using eg tjr_lib/config) |
