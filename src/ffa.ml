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

  let rec fp2 id2 = function
    | [] -> []
    | id :: rest -> 
      if List.mem id forbidden 
      then fp2 id2 rest 
      else
      if (id = id2)
      then [id]
      else match (find_path gr (id :: forbidden) id id2) with 
        | None -> fp2 id2 rest
        | Some x -> id :: x in

  let res = match (n_list id1) with
    | [] -> []
    | x -> match (fp2 id2 x) with
      | [] -> []
      | x -> x in

  match res with
  | [] -> None
  | x -> Some x