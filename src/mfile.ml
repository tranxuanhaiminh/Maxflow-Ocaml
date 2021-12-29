open Graph
open Printf

type path = string

(* Reads the avg of the amount of money paid *)
let read_avg line =
  try Scanf.sscanf line "s %f" (fun x -> x)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Ensure that the given node exists in the graph. If not, create it. 
 * (Necessary because the website we use to create online graphs does not generate correct files when some nodes have been deleted.) *)
let ensure graph id = if node_exists graph id then graph else new_node graph id

(* Reads a line with a node. *)
let read_node id graph line avg =
  try Scanf.sscanf line "n %f" (fun x ->
    let diff = Float.sub x avg in
    let graph2 = ensure (ensure (ensure graph id) 0) 1 in
    if (<) diff 0. then new_arc graph2 0 id (Float.to_string (Float.abs diff))
    else new_arc graph2 id 1 (Float.to_string diff))
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a comment or fail. *)
let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

let money_file path =

  let infile = open_in path in

  (* Read all lines until end of file. 
   * n is the current node counter. *)
  let rec loop n graph avg =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let (n2, graph2, avg2) =
        (* Ignore empty lines *)
        if line = "" then (n, graph, avg)

        (* The first character of a line determines its content : n or e. *)
        else match line.[0] with
          | 's' -> (n, graph, read_avg line)
          | 'n' -> (n+1, read_node n graph line avg, avg)

          (* It should be a comment, otherwise we complain. *)
          | _ -> (n, read_comment graph line, avg)
      in      
      loop n2 graph2 avg2

    with End_of_file -> (n, graph, avg) (* Done *)
  in

  let rec loop2 n max acu graph sum =
    if n = max then graph
    else if acu = max then loop2 (n + 1) max (n + 1) graph sum
    else if n = acu then loop2 n max (acu + 1) graph sum
    else loop2 n max (acu + 1) (new_arc (new_arc graph acu n sum) n acu sum) sum in

  let phase1_graph = new_node (new_node empty_graph 0) 1 in

  let (max, phase2_graph, avg) = loop 2 phase1_graph 0.0 in

  let sum = Float.mul (Int.to_float (max - 2)) avg in

  let final_graph = loop2 2 max 2 phase2_graph (Float.to_string sum) in

  close_in infile ;
  final_graph
