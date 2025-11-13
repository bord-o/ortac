(** Transient execution of generated QCheck tests *)

exception Unsafe_cleanup of string
(** Exception raised when safety checks fail during cleanup *)

val execute : library_name:string -> mli_path:string -> unit
(** [execute ~library_name ~mli_path] generates QCheck tests from the Gospel
    specifications in [mli_path], builds them transiently in a temporary
    directory within the project, executes the tests, and cleans up.

    The [library_name] parameter specifies which dune library contains the
    module being tested. The module name is inferred from the .mli filename.

    The function will:
    1. Find the dune project root
    2. Create a temporary directory in the project root: ortac-qcheck-pbt-XXXXXX
    3. Generate dune file in the temp directory (no dune-project)
    4. Generate test.ml with the QCheck tests
    5. Build the tests from project root (temp dir becomes part of workspace)
    6. Execute the tests using 'dune exec'
    7. Clean up the temporary directory
    8. Exit with the test result code

    The temporary directory is created in the project root without a dune-project
    file, making it part of the parent dune workspace. This allows access to both
    public and private libraries within the project. The directory is automatically
    removed after execution, providing transient test execution with no permanent
    project pollution.

    @raise Failure if no dune-project is found
    @raise Unsafe_cleanup if cleanup safety checks fail *)
