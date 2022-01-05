open Graph
open Printf

type path = string

(* Ensure that the given node exists in the graph. If not, create it. 
 * (Necessary because the website we use to create online graphs does not generate correct files when some nodes have been deleted.) *)
let ensure graph id = if node_exists graph id then graph else new_node graph id

(* Read a person's payment and compare with avg to determine where to put that person in the graph as a node *)
let read_node id graph line avg inlay outlay=
  try Scanf.sscanf line "n %f" (fun x ->
    let diff = Float.sub x avg in
    if (=) diff 0. then (graph, inlay, outlay)
    else
    let graph2 = ensure (ensure (ensure graph id) 0) 1 in
    if (<) diff 0. then (new_arc graph2 0 id (Float.to_string (Float.abs diff)), id :: inlay, outlay)
    else (new_arc graph2 id 1 (Float.to_string diff)), inlay, id :: outlay)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a comment or fail. *)
let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

(* Read each person's payment to get the total payment *)
let read_sum line sum =
  try Scanf.sscanf line "n %f" (fun x -> Float.add sum x)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

let money_file path =

  let infile1 = open_in path in

  (* First loop to get the sum -> average *)
  let rec loop1 n sum =
    try
      let line = input_line infile1 in
      let line = String.trim line in
      let (n2, sum2) =
      if line = "" then (n, sum)
      else match line.[0] with
        | 'n' -> ((Float.add n 1.), read_sum line sum)
        | _ -> (n, sum)
      in
    loop1 n2 sum2
    with End_of_file -> (n, sum)
  in

  (* Obtain the average *)
  let (n, sum) = loop1 1. 0. in

  let avg = Float.div sum (Float.sub n 1.) in

  close_in infile1 ;


  let infile = open_in path in

  (* Read and build graph based on the average value *)
  let rec loop n (graph, inlay, outlay)  =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let (n2, (graph2, inlay2, outlay2)) =
        (* Ignore empty lines *)
        if line = "" then (n, (graph, inlay, outlay))

        (* The first character of a line determines its content : n or e. *)
        else match line.[0] with
          | 'n' -> (n+1, read_node n graph line avg inlay outlay)

          (* It should be a comment, otherwise we complain. *)
          | _ -> (n, (read_comment graph line, inlay, outlay))
      in      
      loop n2 (graph2, inlay2, outlay2)

    with End_of_file -> (n, (graph, inlay, outlay)) (* Done *)
  in

  (* A loop through the graph to create arcs from each node in layer inlay to layer outlay *)
  let rec loop2 graph sum inlay outlay =
    match inlay with
      | [] -> graph
      | x :: rest -> loop2 (List.fold_left (fun a b -> new_arc a x b sum) graph outlay) sum rest outlay in

  (* Create graph with only 2 nodes *)
  let phase1_graph = new_node (new_node empty_graph 0) 1 in

  (* Create graph with all people as nodes connect to the 2 first nodes *)
  let (max, (phase2_graph, inlay, outlay)) = loop 2 (phase1_graph, [], []) in

  (* The final graph *)
  let final_graph = loop2 phase2_graph (Float.to_string sum) inlay outlay in

  close_in infile ;
  (final_graph, inlay, outlay, sum)

(* Write to outfile the result of the payment *)
let payback path graph inlay outlay avg =

  let ff = open_out path in
  
  List.iter (fun a -> List.iter (fun b -> match (find_arc graph a b) with
    | Some x -> 
      let sub = Float.sub avg x in
      if sub = 0. then fprintf ff ""
      else fprintf ff "Number %d has to pay number %d back %f [currency unit]\n" (a - 1) (b - 1) (Float.sub avg x)
    | None -> fprintf ff "") outlay) inlay;

  close_out ff;
  ()