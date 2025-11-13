module W = Ortac_core.Warnings
open Ppxlib
open Ortac_core.Builder

(** Error types for generator creation *)
exception Unsupported_custom_type of string
exception Too_many_arguments of int
exception Unsupported_nested_type of string

(** Generate a QCheck generator expression for a given IR type.

    For polymorphic types (list, option, array, etc.), instantiates with int.
    Emits warnings for polymorphic instantiation.
    Raises generator_error for unsupported custom types. *)
let type_to_generator (t : Ir.type_) : expression =
  (* Helper to build QCheck.<name> *)
  let qcheck_gen name =
    let lid = Ldot (Lident "QCheck", name) in
    pexp_ident { txt = lid; loc = Location.none }
  in

  (* Helper to build QCheck.(combinator arg) *)
  let qcheck_gen_apply combinator arg =
    pexp_apply (qcheck_gen combinator) [(Nolabel, arg)]
  in

  match t.name with
  (* Basic types *)
  | "int" | "integer" -> qcheck_gen "int"
  | "bool" -> qcheck_gen "bool"
  | "string" -> qcheck_gen "string"
  | "char" -> qcheck_gen "char"
  | "float" -> qcheck_gen "float"
  | "unit" -> qcheck_gen "unit"

  (* Polymorphic types - instantiate with int *)
  | "list" ->
      (* Generate: QCheck.(list int) *)
      Fmt.epr "Warning: Instantiating polymorphic type 'list' with 'int'@.";
      qcheck_gen_apply "list" (qcheck_gen "int")

  | "option" ->
      Fmt.epr "Warning: Instantiating polymorphic type 'option' with 'int'@.";
      qcheck_gen_apply "option" (qcheck_gen "int")

  | "array" ->
      Fmt.epr "Warning: Instantiating polymorphic type 'array' with 'int'@.";
      qcheck_gen_apply "array" (qcheck_gen "int")

  | "ref" ->
      Fmt.epr "Warning: Instantiating polymorphic type 'ref' with 'int'@.";
      qcheck_gen_apply "make" (* QCheck.make for refs *) (qcheck_gen "int")

  (* Gospel stdlib types - map to OCaml equivalents *)
  | "sequence" ->
      Fmt.epr "Warning: Mapping 'sequence' to 'list int'@.";
      qcheck_gen_apply "list" (qcheck_gen "int")

  | "set" ->
      Fmt.epr "Warning: Mapping 'set' to 'list int' (no native set generator)@.";
      qcheck_gen_apply "list" (qcheck_gen "int")

  | "bag" ->
      Fmt.epr "Warning: Mapping 'bag' to 'list int' (no native bag generator)@.";
      qcheck_gen_apply "list" (qcheck_gen "int")

  (* Unsupported custom types *)
  | name ->
      raise (Unsupported_custom_type name)

(** Combine multiple generators for multi-argument functions.

    Supports up to 4 arguments using QCheck's built-in combinators:
    - 1 arg: returns generator as-is
    - 2 args: QCheck.pair
    - 3 args: QCheck.triple
    - 4 args: QCheck.quad

    Raises Too_many_arguments for 5+ arguments. *)
let combine_generators (gens : expression list) : expression =
  (* Helper to build QCheck.<combinator> gen1 gen2 ... *)
  let qcheck_combine name args =
    let lid = Ldot (Lident "QCheck", name) in
    let combinator = pexp_ident { txt = lid; loc = Location.none } in
    pexp_apply combinator (List.map (fun e -> (Nolabel, e)) args)
  in

  match gens with
  | [] ->
      (* No arguments - generate unit *)
      let lid = Ldot (Lident "QCheck", "unit") in
      pexp_ident { txt = lid; loc = Location.none }
  | [gen] ->
      (* Single argument - pass through *)
      gen
  | [gen1; gen2] ->
      (* Two arguments - use pair *)
      qcheck_combine "pair" [gen1; gen2]
  | [gen1; gen2; gen3] ->
      (* Three arguments - use triple *)
      qcheck_combine "triple" [gen1; gen2; gen3]
  | [gen1; gen2; gen3; gen4] ->
      (* Four arguments - use quad *)
      qcheck_combine "quad" [gen1; gen2; gen3; gen4]
  | _ ->
      (* 5+ arguments not supported *)
      raise (Too_many_arguments (List.length gens))

(** Format a generator exception as a user-friendly warning message *)
let format_exception = function
  | Unsupported_custom_type name ->
      Printf.sprintf
        "Custom type '%s' is not supported in MVP. \
         Only built-in OCaml types (int, string, bool, list, option, etc.) are supported."
        name
  | Too_many_arguments n ->
      Printf.sprintf
        "Functions with %d arguments are not supported. \
         Maximum is 4 arguments (QCheck.quad limitation)."
        n
  | Unsupported_nested_type name ->
      Printf.sprintf
        "Nested type '%s' is not supported. \
         Only simple type constructors are supported in MVP."
        name
  | exn -> raise exn  (* Re-raise unknown exceptions *)
