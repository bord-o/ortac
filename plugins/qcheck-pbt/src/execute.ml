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

(** Create a temporary directory in /tmp *)
let create_temp_dir () =
  let random_id = Printf.sprintf "%x" (Random.int 0xFFFFFF) in
  let temp_dir =
    Filename.concat (Filename.get_temp_dir_name ()) ("ortac-qcheck-pbt-" ^ random_id)
  in
  Unix.mkdir temp_dir 0o755;
  (temp_dir, random_id)

(** Generate the dune file for the test executable *)
let generate_dune temp_dir library_name random_id =
  let exe_name = "ortac_qcheck_pbt_" ^ random_id in
  let dune_content = Printf.sprintf
{|(executable
 (name %s)
 (libraries qcheck-core qcheck-core.runner ortac-runtime %s))
|}
    exe_name library_name
  in
  let dune_file = Filename.concat temp_dir "dune" in
  let oc = open_out dune_file in
  output_string oc dune_content;
  close_out oc;
  exe_name

(** Generate the dune-project file *)
let generate_dune_project temp_dir =
  let dune_project_content = "(lang dune 3.0)\n" in
  let dune_project_file = Filename.concat temp_dir "dune-project" in
  let oc = open_out dune_project_file in
  output_string oc dune_project_content;
  close_out oc

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
let run_command ~cwd ?ocamlpath cmd =
  let env_prefix = match ocamlpath with
    | Some path -> Printf.sprintf "env OCAMLPATH=%s " (Filename.quote path)
    | None -> ""
  in
  let full_cmd = Printf.sprintf "cd %s && %s%s 2>&1"
    (Filename.quote cwd) env_prefix cmd
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

  (* Safety check 3: Must be in /tmp *)
  let parent = Filename.dirname dir in
  if parent <> Filename.get_temp_dir_name () then
    raise (Unsafe_cleanup
      (Printf.sprintf "SAFETY: Refusing to delete outside /tmp: %s" dir));

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
  (* Find project root and convert to absolute path *)
  let project_root = find_dune_project_root mli_path in
  let abs_project_root =
    if Filename.is_relative project_root then
      if project_root = "." then
        Sys.getcwd ()
      else
        Filename.concat (Sys.getcwd ()) project_root
    else
      project_root
  in
  Fmt.epr "Found dune project root: %s@." abs_project_root;

  (* Extract module name from filename *)
  let module_name =
    mli_path
    |> Filename.basename
    |> Filename.remove_extension
    |> String.capitalize_ascii
  in
  Fmt.epr "Module name: %s@." module_name;

  (* Build the project to ensure library files exist *)
  Fmt.epr "Building project...@.";
  let (build_proj_exit, build_proj_output) = run_command ~cwd:abs_project_root "dune build" in
  if build_proj_exit <> 0 then begin
    Fmt.epr "Failed to build project:@.%s@." build_proj_output;
    exit build_proj_exit
  end;
  Fmt.epr "Project built@.@.";

  (* Compute OCAMLPATH pointing to actual library files in _build/default/lib *)
  let ocamlpath = Filename.concat abs_project_root "_build/default/lib" in

  (* Create temporary directory in /tmp *)
  let (temp_dir, random_id) = create_temp_dir () in
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
      (* Generate dune-project *)
      generate_dune_project temp_dir;
      Fmt.epr "Generated dune-project@.";

      (* Generate dune file *)
      let exe_name = generate_dune temp_dir library_name random_id in
      Fmt.epr "Generated dune file@.";

      (* Generate test.ml *)
      generate_test_ml temp_dir mli_path module_name exe_name;
      Fmt.epr "Generated %s.ml@.@." exe_name;

      (* Build from temp dir with OCAMLPATH pointing to user's libraries *)
      Fmt.epr "Building tests...@.";
      let build_cmd = Printf.sprintf "dune build %s.exe" exe_name in
      let (build_exit, build_output) = run_command ~cwd:temp_dir ~ocamlpath build_cmd in

      if build_exit <> 0 then begin
        Fmt.epr "Build failed:@.%s@." build_output;
        exit build_exit
      end;

      Fmt.epr "Build succeeded@.@.";

      (* Execute tests using dune exec with OCAMLPATH *)
      Fmt.epr "Running tests...@.@.";
      let exec_cmd = Printf.sprintf "dune exec ./%s.exe" exe_name in
      let (test_exit, test_output) = run_command ~cwd:temp_dir ~ocamlpath exec_cmd in

      (* Print test output *)
      print_endline test_output;

      (* Exit with test result code *)
      exit test_exit
    )
