open Gfile
open Tools
open Ffa

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  let _graph2 = gmap graph int_of_string in
  let _graph3 = add_arc _graph2 0 2 3 in
  let _graph4 = gmap _graph3 string_of_int in
  let _graph5 = find_path _graph2 [] 3 4 in

  (* Rewrite the graph that has been read. *)
  let () = export_path outfile (Option.get _graph5) in

  ()

