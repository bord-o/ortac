open Cmdliner
open Registration

(* reexpose Generate module as it is used by the monolith plugin for example *)
module Generate = Generate

module Plugin : sig
  val cmd : unit Cmd.t
end = struct
  let main input output library () =
    match library with
    | Some lib_name ->
        (* Transient execution mode *)
        (try Execute.execute ~library_name:lib_name ~mli_path:input
        with Gospel.Warnings.Error e ->
          Fmt.epr "%a@." Gospel.Warnings.pp e;
          exit 1)
    | None ->
        (* Code generation mode *)
        let fmt = get_out_formatter output in
        (try Generate.generate input fmt
        with Gospel.Warnings.Error e ->
          Fmt.epr "%a@." Gospel.Warnings.pp e;
          exit 1)

  let library =
    let docv = "LIBRARY" in
    Arg.(
      value
      & opt (some string) None
      & info [ "l"; "library" ] ~docv
          ~doc:
            "Execute the generated tests transiently using LIBRARY as the \
             library containing the module under test. If not provided, the \
             generated code is printed to stdout or the output file.")

  let info =
    Cmd.info "qcheck-pbt"
      ~doc:
        "Generate and optionally execute QCheck property-based tests from \
         Gospel specifications"

  let term = Term.(const main $ ocaml_file $ output_file $ library $ setup_log)
  let cmd = Cmd.v info term
end

let () = register Plugin.cmd
