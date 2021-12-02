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

  let rec append l1 = function
    | x :: rest -> if (List.mem x l1) then append l1 rest else append (List.append l1 [x]) rest
    | [] -> l1 in

  let rec loop = function
    | (x :: rest, acu) ->
    if List.mem x acu
    then loop (append rest (n_list x), append acu (n_list x))
    else loop (append rest (n_list x), append acu (n_list x))
    | ([], acu) -> acu in

  match (loop ([3], [3])) with
    | [] -> None
    | x -> Some x








  (* match (out_arcs gr id1) with
  | (x, _) :: rest -> if (!List.mem x forbidden) then find_path gr (id1 :: forbidden) id1 id2
  | [] ->  *)
  


  (* let rec loop (acu, forbidden) (id, _) =

  if List.mem id forbidden
  then List.fold_left loop (acu, forbidden) (out_arcs gr id)
  else List.fold_left loop (id :: acu, id :: forbidden) (out_arcs gr id)
  in

  match (loop ([], []) (id1, 0)) with
    | (_, []) -> None
    | (_, x) -> Some x *)


(* let bfs ids idf gr noeuds_accu =
  if List.exists (fun (x, _) -> x = idf) (List.hd noeuds_accu)
  then 
  else 

    x = [0]
      while each of out_arcs of last of x not idf
            add out_arcs not yet in x to x
    x = [0;1;2;3]

                                           ret idf :: x

          x = 0
                                           loop id x bool = 
                                         if bool then y = out_arcs id that not yet in x *)