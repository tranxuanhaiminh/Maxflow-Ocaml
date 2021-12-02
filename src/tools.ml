(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
(* token: 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 *)
let clone_nodes gr = n_fold gr new_node empty_graph

let gmap gr f = 
  let transform_arc_and_add_to_new_graph gr_accu id1 id2 label = new_arc gr_accu id1 id2 (f label) in
  e_fold gr transform_arc_and_add_to_new_graph (clone_nodes gr)

let add_arc gr id1 id2 i =
  let existing_arc = match (find_arc gr id1 id2) with
    | None -> 0
    | Some x -> x in
  new_arc gr id1 id2 (i + existing_arc)