(** Transient execution of generated QCheck tests *)

exception Unsafe_cleanup of string
(** Exception raised when safety checks fail during cleanup *)

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
