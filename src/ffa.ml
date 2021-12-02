(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
(* token: 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 *)

(* let find_path gr forbidden id1 id2 = match (out_arcs gr id1) with
   | (id, _) :: rest ->  *)



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