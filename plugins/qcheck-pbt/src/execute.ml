(** Transient execution of generated QCheck tests *)

(** Exception raised when safety checks fail during cleanup *)
exception Unsafe_cleanup of string

(** Find the dune project root by walking up the directory tree *)
let rec find_dune_project_root path =
  let dir =
    if Sys.is_directory path then path
    else Filename.dirname path
  in
  let dune_project = Filename.concat dir "dune-project" in
  if Sys.file_exists dune_project then
    dir
  else if dir = "/" || dir = "." || dir = Filename.dirname dir then
    failwith
      "No dune-project found. Please run ortac qcheck-pbt from within a dune project."
  else
    find_dune_project_root (Filename.dirname dir)

(** Create a temporary directory in the project root *)
let create_temp_dir project_root =
  let random_id = Printf.sprintf "%x" (Random.int 0xFFFFFF) in
  let temp_dir =
    Filename.concat project_root ("ortac-qcheck-pbt-" ^ random_id)
  in
  Unix.mkdir temp_dir 0o755;
  (temp_dir, random_id)

(** Generate the dune file for the test executable *)
let generate_dune temp_dir library_name random_id =
  let exe_name = "ortac_qcheck_pbt_" ^ random_id in
  let dune_content = Printf.sprintf
{|(executable
 (name %s)
 (public_name %s)
 (libraries qcheck-core qcheck-core.runner ortac-runtime %s))
|}
    exe_name exe_name library_name
  in
  let dune_file = Filename.concat temp_dir "dune" in
  let oc = open_out dune_file in
  output_string oc dune_content;
  close_out oc;
  exe_name

(** Generate the test.ml file with generated test code *)
let generate_test_ml temp_dir mli_path module_name exe_name =
  let test_file = Filename.concat temp_dir (exe_name ^ ".ml") in
  let oc = open_out test_file in
  let fmt = Format.formatter_of_out_channel oc in

  (* Add the include statement *)
  Format.fprintf fmt "include %s@.@." module_name;

  (* Generate the test code *)
  let (env, sigs) =
    Gospel.Parser_frontend.parse_ocaml_gospel mli_path
    |> Ortac_core.Utils.type_check [] mli_path
  in
  assert (List.length env = 1);
  let namespace = List.hd env in
  let structure = Generate.signature ~runtime:"Ortac_runtime" ~module_name namespace sigs in

  Ppxlib.Pprintast.structure fmt structure;
  Format.pp_print_flush fmt ();
  close_out oc

(** Run a command and return its exit code and output *)
let run_command ~cwd cmd =
  let full_cmd = Printf.sprintf "cd %s && %s 2>&1"
    (Filename.quote cwd) cmd
  in
  let ic = Unix.open_process_in full_cmd in
  let buf = Buffer.create 1024 in
  (try
    while true do
      Buffer.add_channel buf ic 1
    done
  with End_of_file -> ());
  let status = Unix.close_process_in ic in
  let output = Buffer.contents buf in
  let exit_code = match status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED _ -> 128
    | Unix.WSTOPPED _ -> 128
  in
  (exit_code, output)

(** Safely remove a temporary directory with multiple safety checks *)
let safe_remove_temp_dir dir =
  (* Safety check 1: Must match our naming pattern *)
  let basename = Filename.basename dir in
  if not (String.starts_with ~prefix:"ortac-qcheck-pbt-" basename) then
    raise (Unsafe_cleanup
      (Printf.sprintf "SAFETY: Refusing to delete %s (wrong pattern)" basename));

  (* Safety check 2: Must exist and be a directory *)
  if not (Sys.file_exists dir && Sys.is_directory dir) then
    raise (Unsafe_cleanup
      (Printf.sprintf "SAFETY: Path %s doesn't exist or isn't a directory" dir));

  (* Safety check 3: Must not be a critical system directory *)
  let parent = Filename.dirname dir in
  if parent = "/" || parent = Filename.get_temp_dir_name () then
    raise (Unsafe_cleanup
      (Printf.sprintf "SAFETY: Refusing to delete in critical location %s" parent));

  (* NOW it's safe to remove *)
  let rec remove_recursive path =
    if Sys.is_directory path then begin
      Sys.readdir path
      |> Array.iter (fun entry ->
          remove_recursive (Filename.concat path entry));
      Unix.rmdir path
    end else
      Sys.remove path
  in
  remove_recursive dir

(** Execute the generated tests transiently *)
let execute ~library_name ~mli_path =
  (* Find project root *)
  let project_root = find_dune_project_root mli_path in
  Fmt.epr "Found dune project root: %s@." project_root;

  (* Extract module name from filename *)
  let module_name =
    mli_path
    |> Filename.basename
    |> Filename.remove_extension
    |> String.capitalize_ascii
  in
  Fmt.epr "Module name: %s@." module_name;

  (* Create temporary directory *)
  let (temp_dir, random_id) = create_temp_dir project_root in
  Fmt.epr "Created temp directory: %s@." temp_dir;

  Fun.protect
    ~finally:(fun () ->
      try
        safe_remove_temp_dir temp_dir;
        Fmt.epr "Cleaned up temp directory@."
      with
      | Unsafe_cleanup msg ->
          Fmt.epr "CLEANUP ERROR: %s@." msg;
          Fmt.epr "Please manually remove: %s@." temp_dir
      | e ->
          Fmt.epr "Warning: Failed to cleanup %s: %s@."
            temp_dir (Printexc.to_string e))
    (fun () ->
      (* Generate dune file (no dune-project - let it be part of parent workspace) *)
      let exe_name = generate_dune temp_dir library_name random_id in
      Fmt.epr "Generated dune file@.";

      (* Generate test.ml *)
      generate_test_ml temp_dir mli_path module_name exe_name;
      Fmt.epr "Generated %s.ml@.@." exe_name;

      (* Build from project root - dune will find the temp dir as part of workspace *)
      Fmt.epr "Building tests...@.";
      let temp_basename = Filename.basename temp_dir in
      let build_cmd = Printf.sprintf "dune build %s/%s.exe" temp_basename exe_name in
      let (build_exit, build_output) = run_command ~cwd:project_root build_cmd in

      if build_exit <> 0 then begin
        Fmt.epr "Build failed:@.%s@." build_output;
        exit build_exit
      end;

      Fmt.epr "Build succeeded@.@.";

      (* Execute tests using dune exec *)
      Fmt.epr "Running tests...@.@.";
      let exec_cmd = Printf.sprintf "dune exec %s" exe_name in
      let (test_exit, test_output) = run_command ~cwd:project_root exec_cmd in

      (* Print test output *)
      print_endline test_output;

      (* Exit with test result code *)
      exit test_exit
    )
