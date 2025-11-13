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
    1. Find the dune project root
    2. Run 'dune build @install' in the project to ensure libraries are available
    3. Create a temporary directory in /tmp: ortac-qcheck-pbt-XXXXXX
    4. Generate dune-workspace pointing to _build/install/default/lib
    5. Generate dune and test.ml in the temp directory
    6. Build the tests using dune (with workspace providing library paths)
    7. Execute the tests
    8. Clean up the temporary directory
    9. Exit with the test result code

    The temporary directory is created in /tmp and uses a dune-workspace file
    to access the user's locally installed (but unpublished) libraries via
    OCAMLPATH, avoiding pollution of the user's project directory.

    @raise Failure if no dune-project is found
    @raise Unsafe_cleanup if cleanup safety checks fail *)
