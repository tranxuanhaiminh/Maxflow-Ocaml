(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

type path = string

(* Read input files to build graph *)
val money_file: path -> (string graph * int list * int list * float)

(* Write result file *)
val payback: path -> float graph -> int list -> int list -> float -> unit