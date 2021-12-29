open Graph


(* A path is a list of nodes. *)
type path = id list

(* find_path gr forbidden id1 id2 
 *   returns None if no path can be found.
 *   returns Some p if a path p from id1 to id2 has been found. 
 *
 *  forbidden is a list of forbidden nodes (they have already been visited)
 *)
val find_path: float graph -> id list -> id -> id -> path option

val min_flow: float graph -> path option -> float

val step: float graph -> float -> path option -> float graph

val ford: float graph -> id -> id -> (float * float graph)