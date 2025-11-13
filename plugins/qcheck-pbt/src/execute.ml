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

(** Create a temporary directory in /tmp/ *)
let create_temp_dir () =
  Random.self_init ();
  let random_id = Printf.sprintf "%x" (Random.int 0xFFFFFF) in
  let temp_dir = Filename.concat "/tmp" ("ortac-qcheck-pbt-" ^ random_id) in
  Unix.mkdir temp_dir 0o755;
  (temp_dir, random_id)

(** Generate the dune file for the test executable *)
let generate_dune temp_dir library_name random_id =
  let exe_name = "ortac_qcheck_pbt_" ^ random_id in
  let public_name = exe_name in
  let dune_content = Printf.sprintf
{|(executable
 (name %s)
 (public_name %s)
 (libraries qcheck-core qcheck-core.runner ortac-runtime %s))
|}
    exe_name public_name library_name
  in
  let dune_file = Filename.concat temp_dir "dune" in
  let oc = open_out dune_file in
  output_string oc dune_content;
  close_out oc;
  exe_name

(** Generate the dune-project file *)
let generate_dune_project temp_dir module_name random_id =
  let package_name = "ortac-qcheck-pbt-" ^ random_id in
  let dune_project_content = Printf.sprintf
{|(lang dune 3.0)
(generate_opam_files true)

(package
 (name %s)
 (synopsis "Generated tests for the %s module"))
|}
    package_name module_name
  in
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

(** Create a symlink to the project root *)
let create_project_symlink temp_dir project_root library_name =
  let symlink_path = Filename.concat temp_dir library_name in
  Unix.symlink ~to_dir:true project_root symlink_path

(** Safely remove a temporary directory with multiple safety checks *)
let safe_remove_temp_dir dir library_name =
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
  if parent <> "/tmp" then
    raise (Unsafe_cleanup
      (Printf.sprintf "SAFETY: Refusing to delete outside /tmp (found in %s)" parent));

  (* Remove symlink first to avoid circular reference during cleanup *)
  let symlink_path = Filename.concat dir library_name in
  if Sys.file_exists symlink_path then
    Sys.remove symlink_path;

  (* NOW it's safe to remove everything else *)
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
  let project_root =
    let root = find_dune_project_root mli_path in
    if Filename.is_relative root then
      Filename.concat (Sys.getcwd ()) root
    else
      root
  in

  (* Extract module name from filename *)
  let module_name =
    mli_path
    |> Filename.basename
    |> Filename.remove_extension
    |> String.capitalize_ascii
  in

  (* Create temporary directory in /tmp/ *)
  let (temp_dir, random_id) = create_temp_dir () in

  let exit_code =
    Fun.protect
      ~finally:(fun () ->
        try
          print_endline "skipping cleanup";
          (* safe_remove_temp_dir temp_dir library_name *)
        with
        | Unsafe_cleanup msg ->
            Fmt.epr "CLEANUP ERROR: %s@." msg;
            Fmt.epr "Please manually remove: %s@." temp_dir
        | e ->
            Fmt.epr "Warning: Failed to cleanup %s: %s@."
              temp_dir (Printexc.to_string e))
      (fun () ->
        (* Create symlink to project root *)
        create_project_symlink temp_dir project_root library_name;

        (* Generate dune-project file *)
        generate_dune_project temp_dir module_name random_id;

        (* Generate dune file *)
        let exe_name = generate_dune temp_dir library_name random_id in

        (* Generate test.ml *)
        generate_test_ml temp_dir mli_path module_name exe_name;

        (* Build tests *)
        let (build_exit, build_output) = run_command ~cwd:temp_dir "dune b" in

        if build_exit <> 0 then begin
          Fmt.epr "Build failed:@.%s@." build_output;
          build_exit
        end else begin
          (* Execute tests *)
          let exec_cmd = Printf.sprintf "dune exe %s" exe_name in
          let (test_exit, test_output) = run_command ~cwd:temp_dir exec_cmd in

          (* Print test output *)
          print_endline test_output;

          (* Return test result code *)
          test_exit
        end
      )
  in
  exit exit_code
