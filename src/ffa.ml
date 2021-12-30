(* Yes, we have to repeat open Graph. *)
open Graph
open Tools

(* assert false is of type ∀α.α, so the type-checker is happy. *)
(* token: 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 *)

(* let find_path gr forbidden id1 id2 = match (out_arcs gr id1) with
   | (id, _) :: rest ->  *)

(* exception Graph_error *)

(* A path is a list of nodes. *)
type path = id list

let rec find_path gr forbidden id1 id2 = 

  let n_list id = 
    let rec ns = function
      | (x, _) :: rest -> x :: ns rest
      | [] -> [] in
    ns (out_arcs gr id) in

  let rec res forbidden id1 id2 = 

    let rec fp2 forbidden id2 = function
      | [] -> []
      | id :: rest -> 
        if List.mem id forbidden 
        then fp2 (id :: forbidden) id2 rest 
        else
        if (id = id2)
        then [id]
        else match (res forbidden id id2) with 
          | None -> fp2 forbidden id2 rest
          | Some x -> id :: x in

    match (n_list id1) with
    | [] -> None
    | x -> match (fp2 (id1 :: forbidden) id2 x) with
      | [] -> None
      | x -> Some x in

  match res forbidden id1 id2 with
  | None -> None
  | Some x -> Some (id1 :: x)



let rec min_flow gr = function
  | None -> 0.
  | Some ([]) -> 0.
  | Some (id1 :: []) -> 0.
  | Some (id1 :: id2 :: rest) -> match (find_arc gr id1 id2) with
    | None -> raise (Graph_error("Path error in path between node" ^ string_of_int id1 ^ " and " ^ string_of_int id2))
    | Some lbl ->
      let next_lbl = min_flow gr (Some(id2 :: rest)) in
      if (lbl < next_lbl) || (next_lbl == 0.) then lbl else next_lbl



let rec step gr min = function
  | None -> gr
  | Some ([]) -> gr
  | Some (id1 :: []) -> gr
  | Some (id1 :: id2 :: rest) ->
    let gr_ecart = add_arc gr id2 id1 min in
    let lbl = Option.get (find_arc gr_ecart id1 id2) in
    (* Printf.printf"min: %f arc from %d to %d has lbl: %f is %b\n" min id1 id2 lbl (min = lbl); *)
    if (lbl = min)
    then step (remove_arc gr_ecart id1 id2) min (Some(id2 :: rest))
    else step (add_arc gr_ecart id1 id2 (Float.neg(min))) min (Some(id2 :: rest))




let rec ford gr id1 id2 = match (find_path gr [] id1 id2) with
  | None -> (0., gr)
  | Some p ->
    let min = min_flow gr (Some p) in
    let graph_ecart = step gr min (Some p) in
    match (ford graph_ecart id1 id2) with
      | (x, graph_ecart2) -> (Float.add min x, graph_ecart2)