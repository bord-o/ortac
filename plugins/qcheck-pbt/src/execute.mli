(** Transient execution of generated QCheck tests *)

exception Unsafe_cleanup of string
(** Exception raised when safety checks fail during cleanup *)

val find_dune_project_root : string -> string
(** [find_dune_project_root path] walks up the directory tree from [path]
    to find the nearest dune-project file and returns its directory path. *)

val create_temp_dir : unit -> string * string
(** [create_temp_dir ()] creates a temporary directory in /tmp/ with a random
    ID and returns [(temp_dir, random_id)]. *)

val generate_dune : string -> string -> string -> string
(** [generate_dune temp_dir library_name random_id] generates a dune file in
    [temp_dir] for an executable that depends on [library_name]. Returns the
    executable name. *)

val generate_dune_project : string -> string -> string -> unit
(** [generate_dune_project temp_dir module_name random_id] generates a
    dune-project file in [temp_dir]. *)

val generate_test_ml : string -> string -> string -> string -> unit
(** [generate_test_ml temp_dir mli_path module_name exe_name] generates the
    test ML file with QCheck tests from the Gospel specifications in [mli_path]. *)

val run_command : cwd:string -> string -> int * string
(** [run_command ~cwd cmd] runs [cmd] in directory [cwd] and returns
    [(exit_code, output)]. *)

val create_project_symlink : string -> string -> string -> unit
(** [create_project_symlink temp_dir project_root library_name] creates a
    symlink in [temp_dir] named [library_name] pointing to [project_root]. *)

val safe_remove_temp_dir : string -> string -> unit
(** [safe_remove_temp_dir dir library_name] safely removes [dir] after removing
    the symlink named [library_name]. Performs safety checks to ensure [dir] is
    in /tmp and matches the expected pattern.
    @raise Unsafe_cleanup if safety checks fail *)

val execute : library_name:string -> mli_path:string -> unit
(** [execute ~library_name ~mli_path] generates QCheck tests from the Gospel
    specifications in [mli_path], builds them transiently in /tmp/, executes
    the tests, and cleans up.

    The [library_name] parameter specifies which dune library contains the
    module being tested. This library MUST have a public_name defined in its
    dune file for the approach to work. The module name is inferred from the
    .mli filename.

    The function will:
    1. Find the dune project root
    2. Create a temporary directory in /tmp/: ortac-qcheck-pbt-XXXXXX
    3. Create a symlink from temp_dir/<library_name> to the project root
    4. Generate dune-project file
    5. Generate dune file with public_name for the executable
    6. Generate test.ml with the QCheck tests
    7. Build the tests with 'dune b' in the temp directory
    8. Execute the tests with 'dune exe <exe_name>' in the temp directory
    9. Clean up the temporary directory from /tmp/
    10. Exit with the test result code

    The temporary directory is created in /tmp/ with its own dune-project,
    making it an independent dune workspace. A symlink to the project root
    allows the build to access the library under test. This provides truly
    transient test execution with no project pollution.

    @raise Failure if no dune-project is found
    @raise Unsafe_cleanup if cleanup safety checks fail *)
