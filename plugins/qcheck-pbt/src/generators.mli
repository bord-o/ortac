open Ppxlib

(** Exceptions for generator creation *)
exception Unsupported_custom_type of string
exception Too_many_arguments of int
exception Unsupported_nested_type of string

(** [type_to_generator type_] converts an IR type to a QCheck generator expression.

    For polymorphic types (list, option, array), instantiates with int and emits warnings.

    @raise Unsupported_custom_type for custom user-defined types *)
val type_to_generator : Ir.type_ -> expression

(** [combine_generators gens] combines multiple generators for multi-argument functions.

    Supports up to 4 arguments using QCheck's built-in combinators (pair, triple, quad).

    @raise Too_many_arguments for 5+ arguments *)
val combine_generators : expression list -> expression

(** [format_exception exn] formats a generator exception as a user-friendly message *)
val format_exception : exn -> string
