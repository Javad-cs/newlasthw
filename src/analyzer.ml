open! ClassroomAdd commentMore actions
module F = Format
module Command = CFG.Command
module ConstraintSet = Constraint.Set

(* ************************************** *
   Extracting Basic Facts
 * ************************************** *)

let extract_cedge src dst set = failwith "Not implemented"
let extract_source node set = failwith "Not implemented"
let extract_sanitizer node set = failwith "Not implemented"
let extract_sink node set = failwith "Not implemented"
let extract_def node set = failwith "Not implemented"
let extract_use node set = failwith "Not implemented"
let extract_kill node1 node2 set = failwith "Not implemented"
let extract : CFG.t -> ConstraintSet.t = fun cfg -> failwith "Not implemented"

(* ************************************** *
   Rules for Reaching Definition Analysis
 * ************************************** *)

let derive_out1 cs = failwith "Not implemented"
let derive_out2 cs = failwith "Not implemented"
let derive_in cs = failwith "Not implemented"

(* ************************************** *
   Rules for Taint Analysis
 * ************************************** *)

let derive_edge cs = failwith "Not implemented"
let derive_path1 cs = failwith "Not implemented"
let derive_path2 cs = failwith "Not implemented"
let derive_alarm cs = failwith "Not implemented"
let rec solve const_set = failwith "Not implemented"