module SourceFile = struct
  type t =
    | Invalid of {ml_source_file: string}
    | Absolute of string
    | RelativeProjectRoot of string  (** relative to project root *)
    | RelativeInferBiabductionModel of string  (** relative to infer models *)
end

module Location = struct
  (** Location in the original source file *)

  type t = {line: int  (** line number. -1 menas "do not know" *); col: int; file: SourceFile.t}
end

module SymOp = struct
  type failure_kind =
    | FKtimeout
    | FKsymops_timeout of int
    | FKrecursion_timeout of int
    | FKcrash of string
end
