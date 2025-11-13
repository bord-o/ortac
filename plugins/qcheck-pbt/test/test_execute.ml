(** Unit tests for the Execute module using simple assertions *)

open Ortac_qcheck_pbt.Execute

(* Simple substring check *)
let contains_substring s sub =
  try
    let len_s = String.length s in
    let len_sub = String.length sub in
    let rec check i =
      if i + len_sub > len_s then false
      else if String.sub s i len_sub = sub then true
      else check (i + 1)
    in
    check 0
  with _ -> false

let test_count = ref 0
let pass_count = ref 0
let fail_count = ref 0

let assert_equal ~msg expected actual =
  test_count := !test_count + 1;
  if expected = actual then begin
    pass_count := !pass_count + 1;
    Printf.printf "✓ %s\n%!" msg
  end else begin
    fail_count := !fail_count + 1;
    Printf.printf "✗ %s\n  Expected: %s\n  Got: %s\n%!"
      msg (Printexc.to_string (Obj.magic expected)) (Printexc.to_string (Obj.magic actual))
  end

let assert_true ~msg condition =
  test_count := !test_count + 1;
  if condition then begin
    pass_count := !pass_count + 1;
    Printf.printf "✓ %s\n%!" msg
  end else begin
    fail_count := !fail_count + 1;
    Printf.printf "✗ %s (expected true, got false)\n%!" msg
  end

let test_find_dune_project_root () =
  Printf.printf "\n=== Testing find_dune_project_root ===\n%!";
  let temp_dir = Filename.temp_dir "test_ortac" "" in
  let dune_project = Filename.concat temp_dir "dune-project" in
  Out_channel.with_open_text dune_project (fun oc ->
    Out_channel.output_string oc "(lang dune 3.0)");

  let found = find_dune_project_root temp_dir in
  assert_equal ~msg:"should find dune-project in current dir" temp_dir found;

  Sys.remove dune_project;
  Unix.rmdir temp_dir

let test_create_temp_dir () =
  Printf.printf "\n=== Testing create_temp_dir ===\n%!";
  let (temp_dir, random_id) = create_temp_dir () in

  assert_true ~msg:"dir starts with /tmp/ortac-qcheck-pbt-"
    (String.starts_with ~prefix:"/tmp/ortac-qcheck-pbt-" temp_dir);
  assert_true ~msg:"dir exists" (Sys.file_exists temp_dir);
  assert_true ~msg:"is directory" (Sys.is_directory temp_dir);
  assert_equal ~msg:"random ID length is 6" 6 (String.length random_id);

  Unix.rmdir temp_dir

let test_create_temp_dir_unique () =
  Printf.printf "\n=== Testing create_temp_dir uniqueness ===\n%!";
  let (dir1, id1) = create_temp_dir () in
  let (dir2, id2) = create_temp_dir () in

  assert_true ~msg:"different IDs" (id1 <> id2);
  assert_true ~msg:"different directories" (dir1 <> dir2);

  Unix.rmdir dir1;
  Unix.rmdir dir2

let test_generate_dune () =
  Printf.printf "\n=== Testing generate_dune ===\n%!";
  let temp_dir = Filename.temp_dir "test_ortac" "" in
  let exe_name = generate_dune temp_dir "mylib" "abc123" in

  assert_equal ~msg:"exe name" "ortac_qcheck_pbt_abc123" exe_name;

  let dune_file = Filename.concat temp_dir "dune" in
  assert_true ~msg:"dune file exists" (Sys.file_exists dune_file);

  let content = In_channel.with_open_text dune_file In_channel.input_all in
  assert_true ~msg:"contains library name" (contains_substring content "mylib");

  Sys.remove dune_file;
  Unix.rmdir temp_dir

let test_generate_dune_project () =
  Printf.printf "\n=== Testing generate_dune_project ===\n%!";
  let temp_dir = Filename.temp_dir "test_ortac" "" in
  generate_dune_project temp_dir "MyModule" "abc123";

  let file = Filename.concat temp_dir "dune-project" in
  assert_true ~msg:"dune-project exists" (Sys.file_exists file);

  let content = In_channel.with_open_text file In_channel.input_all in
  assert_true ~msg:"contains package name"
    (contains_substring content "ortac-qcheck-pbt-abc123");
  assert_true ~msg:"contains module name in synopsis"
    (contains_substring content "MyModule");

  Sys.remove file;
  Unix.rmdir temp_dir

let test_create_project_symlink () =
  Printf.printf "\n=== Testing create_project_symlink ===\n%!";
  let temp_dir = Filename.temp_dir "test_ortac" "" in
  let project_dir = Filename.temp_dir "test_project" "" in
  create_project_symlink temp_dir project_dir "mylib";

  let symlink = Filename.concat temp_dir "mylib" in
  assert_true ~msg:"symlink exists" (Sys.file_exists symlink);

  let target = Unix.readlink symlink in
  assert_equal ~msg:"symlink points to project dir" project_dir target;

  Sys.remove symlink;
  Unix.rmdir temp_dir;
  Unix.rmdir project_dir

let test_safe_remove_temp_dir () =
  Printf.printf "\n=== Testing safe_remove_temp_dir ===\n%!";
  let temp_dir = Filename.concat "/tmp" "ortac-qcheck-pbt-test456" in
  let project_dir = Filename.temp_dir "test_project" "" in
  Unix.mkdir temp_dir 0o755;

  create_project_symlink temp_dir project_dir "mylib";
  let test_file = Filename.concat temp_dir "test.txt" in
  Out_channel.with_open_text test_file (fun oc ->
    Out_channel.output_string oc "test");

  assert_true ~msg:"dir exists before cleanup" (Sys.file_exists temp_dir);

  safe_remove_temp_dir temp_dir "mylib";

  assert_true ~msg:"dir removed after cleanup" (not (Sys.file_exists temp_dir));
  assert_true ~msg:"project dir still exists" (Sys.file_exists project_dir);

  Unix.rmdir project_dir

let test_safe_remove_refuses_wrong_pattern () =
  Printf.printf "\n=== Testing safe_remove_temp_dir safety checks ===\n%!";
  let temp_dir = Filename.concat "/tmp" "wrong-pattern-test789" in
  Unix.mkdir temp_dir 0o755;

  let raised_exception = ref false in
  (try
    safe_remove_temp_dir temp_dir "mylib"
  with
  | Unsafe_cleanup _ -> raised_exception := true);

  assert_true ~msg:"raises Unsafe_cleanup for wrong pattern" !raised_exception;

  Unix.rmdir temp_dir

let test_run_command () =
  Printf.printf "\n=== Testing run_command ===\n%!";
  let temp_dir = Filename.temp_dir "test_ortac" "" in
  let (exit_code, output) = run_command ~cwd:temp_dir "echo 'hello world'" in

  assert_equal ~msg:"exit code is 0" 0 exit_code;
  assert_true ~msg:"output contains hello world"
    (contains_substring output "hello world");

  Unix.rmdir temp_dir

let () =
  Printf.printf "\n========================================\n";
  Printf.printf "Running Execute module tests\n";
  Printf.printf "========================================\n%!";

  test_find_dune_project_root ();
  test_create_temp_dir ();
  test_create_temp_dir_unique ();
  test_generate_dune ();
  test_generate_dune_project ();
  test_create_project_symlink ();
  test_safe_remove_temp_dir ();
  test_safe_remove_refuses_wrong_pattern ();
  test_run_command ();

  Printf.printf "\n========================================\n";
  Printf.printf "Tests complete: %d total, %d passed, %d failed\n"
    !test_count !pass_count !fail_count;
  Printf.printf "========================================\n%!";

  if !fail_count > 0 then exit 1
