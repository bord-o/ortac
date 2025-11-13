(** Transient execution of generated QCheck tests *)

exception Unsafe_cleanup of string
(** Exception raised when safety checks fail during cleanup *)

val execute : library_name:string -> mli_path:string -> unit
(** [execute ~library_name ~mli_path] generates QCheck tests from the Gospel
    specifications in [mli_path], builds them in a temporary directory,
    executes the tests, and cleans up.

    The [library_name] parameter specifies which dune library contains the
    module being tested. The module name is inferred from the .mli filename.

    The function will:
    1. Find the dune project root
    2. Create a temporary directory: ortac-qcheck-pbt-XXXXXX
    3. Generate dune and test.ml in the temp directory
    4. Build the tests using dune (as part of the parent workspace)
    5. Execute the tests
    6. Clean up the temporary directory
    7. Exit with the test result code

    The temporary directory is created in the project root and becomes part
    of the parent workspace, allowing it to reference local libraries.

    @raise Failure if no dune-project is found
    @raise Unsafe_cleanup if cleanup safety checks fail *)
