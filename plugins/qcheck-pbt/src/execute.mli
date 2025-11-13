(** Transient execution of generated QCheck tests *)

exception Unsafe_cleanup of string
(** Exception raised when safety checks fail during cleanup *)

val execute : library_name:string -> mli_path:string -> unit
(** [execute ~library_name ~mli_path] generates QCheck tests from the Gospel
    specifications in [mli_path], builds them in a temporary directory in /tmp,
    executes the tests, and cleans up.

    The [library_name] parameter specifies which dune library contains the
    module being tested. The module name is inferred from the .mli filename.

    The function will:
    1. Find the dune project root and convert to absolute path
    2. Run 'dune build' in the project to ensure library files exist
    3. Create a temporary directory in /tmp: ortac-qcheck-pbt-XXXXXX
    4. Generate dune-project and dune files in the temp directory
    5. Generate test.ml with the QCheck tests
    6. Build the tests using 'dune build' with OCAMLPATH set to _build/default/lib
    7. Execute the tests using 'dune exec' with OCAMLPATH
    8. Clean up the temporary directory
    9. Exit with the test result code

    The temporary directory is created in /tmp and OCAMLPATH is set as an
    environment variable when running build/exec commands, pointing to
    _build/default/lib where the actual library files reside. This avoids
    pollution of the user's project directory while allowing access to
    locally built libraries.

    @raise Failure if no dune-project is found
    @raise Unsafe_cleanup if cleanup safety checks fail *)
