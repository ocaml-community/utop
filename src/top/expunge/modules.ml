open StdLabels

let input_lines ic =
  let rec loop acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | line -> loop (line :: acc)
  in
  loop []

let lines_of_info fn =
  let ic = open_in fn in
  Fun.protect
    (fun () -> input_lines ic)
    ~finally:(fun () -> close_in_noerr ic)

module S = Set.Make(String)

let main ~src ~cma_files =
  let modules =
    lines_of_info src
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
      lines_of_info fn
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
  let modules_to_keep = S.diff modules modules_to_exclude in
  S.iter print_endline modules_to_keep

let () =
  match Array.to_list Sys.argv with
  | _ :: src :: cma_files ->
    main ~src ~cma_files
  | _ ->
    failwith "invalid command line"
