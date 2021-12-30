open Gfile
open Tools
open Ffa
open Mfile

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
  (* let graph = from_file infile in

  let _graph2 = gmap graph int_of_string in
  let _graph3 = add_arc _graph2 0 2 3 in
  let _graph4 = gmap _graph3 string_of_int in
  let _dfs_path = (find_path _graph2 [] 0 4) in
  let _min_f = min_flow _graph2 _dfs_path in
  let _step = step _graph2 _min_f _dfs_path in
  let _string_step = gmap _step string_of_int in
  let _ford = ford _graph2 0 5 in *)

  (* Rewrite the graph that has been read. *)
  (* let () = export outfile graph in *)
  (* let () = export outfile _string_step in *)
  (* let () = export_path outfile (Option.get _dfs_path) in *)
  (* let () = export_num outfile _ford in *)

  let (_graph5, inlay, outlay, avg) = money_file infile in
  let _graph2 = gmap _graph5 Float.of_string in

  let _dfs_path = (find_path _graph2 [] 0 1) in
  let _min_f = min_flow _graph2 _dfs_path in
  let _step = step _graph2 _min_f _dfs_path in

  let _dfs_path2 = find_path _step [] 0 1 in
  let _min_f2 = min_flow _step _dfs_path2 in
  let _step2 = step _step _min_f2 _dfs_path2 in
  
  let _dfs_path3 = find_path _step2 [] 0 1 in
  let _min_f3 = min_flow _step2 _dfs_path3 in
  let _step3 = step _step2 _min_f3 _dfs_path3 in

  let (_ford, _money_graph) = ford _graph2 0 1 in
  let _money_graph2 = gmap _money_graph Float.to_string in
  
  let () = payback outfile _money_graph inlay outlay avg in

  (* let () = write_file outfile _graph5 in *)
  (* let () = write_file2 outfile _step in *)
  (* let () = export_path outfile (Option.get _dfs_path) in *)
  (* let () = export_float outfile _min_f in *)
  (* let () = export2 outfile _step2 in *)

  ()
