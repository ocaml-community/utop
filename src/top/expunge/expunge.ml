open StdLabels
open Printf

let run_and_read_lines args =
  let cmd = String.concat ~sep:" " (List.map args ~f:Filename.quote) in
  let ic = Unix.open_process_in cmd in
  let rec loop acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | line -> loop (line :: acc)
  in
  let x = loop [] in
  match Unix.close_process_in ic with
  | WEXITED 0 -> x
  | WEXITED n ->
    eprintf "Process `%s' exited with code %d\n%!" cmd n;
    exit 1
  | WSIGNALED n ->
    eprintf "Process `%s' got signal %d\n%!" cmd n;
    exit 1
  | WSTOPPED _ -> assert false

module S = Set.Make(String)

let main ~objinfo ~stdlib_dir ~src ~dst ~cma_files ~verbose =
  let modules =
    run_and_read_lines [objinfo; src]
    |> List.map ~f:(fun line ->
      try
        Scanf.sscanf line "\t%[0-9a-f]\t%s"
          (fun a b -> assert (String.length a = 32); [b])
      with _ -> [])
    |> List.concat
    |> S.of_list
  in
  let modules_to_exclude =
    List.map cma_files ~f:(fun fn ->
      run_and_read_lines [objinfo; fn]
      |> List.map ~f:(fun line ->
        try
          Scanf.sscanf line "Unit name: %s" (fun s -> [s])
        with _ -> [])
      |> List.concat)
    |> List.concat
    |> S.of_list
    |> S.remove "Topmain"
    |> S.remove "Toploop"
    |> S.remove "Topdirs"
  in
  if verbose then begin
    eprintf "Modules from the compiler:\n";
    List.iter (S.elements modules_to_exclude) ~f:(eprintf "- %s\n")
  end;
  let modules_to_keep = S.diff modules modules_to_exclude in
  let cmdline =
    sprintf
      "%s %s %s %s"
      (Filename.quote (Filename.concat stdlib_dir "expunge"))
      (Filename.quote src)
      (Filename.quote dst)
      (String.concat ~sep:" " (S.elements modules_to_keep))
  in
  if verbose then prerr_endline cmdline;
  exit (Sys.command cmdline)

let () =
  match Array.to_list Sys.argv with
  | _ :: "-v" :: objinfo :: stdlib_dir :: src :: dst :: cma_files ->
    main ~objinfo ~stdlib_dir ~src ~dst ~cma_files ~verbose:true
  | _ :: objinfo :: stdlib_dir :: src :: dst :: cma_files ->
    main ~objinfo ~stdlib_dir ~src ~dst ~cma_files ~verbose:false
  | _ ->
    failwith "invalid command line"
