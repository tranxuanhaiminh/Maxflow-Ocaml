(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
(* token: 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 *)

(* let find_path gr forbidden id1 id2 = match (out_arcs gr id1) with
| (id, _) :: rest ->  *)



(* A path is a list of nodes. *)
type path = id list

let find_path gr forbidden id1 id2 =
let rec loop acu (id, _) = List.fold_left loop (id :: acu) (out_arcs gr id) in
loop [] (id1, 0)


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