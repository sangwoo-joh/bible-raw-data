module Ident = struct
  (** Module for Names and Identifiers *)
  module Name = struct
    type t = Primed | Normal | Footprint | Spec | FromString of string
  end

  type name = Name.t

  type kind =
    | KNone
        (** special kind of "null ident" (basically, a more compact way of
       implementing an ident option) *)
    | KFootprint
    | KNormal
    | KPrimed

  type t = {kind: kind; name: Name.t; stamp: int}
end

module SourceFile = struct
  type t =
    | Invalid of {ml_source_file: string}
    | Absolute of string
    | RelativeProjectRoot of string  (** relative to project root *)
    | RelativeInferBiabductionModel of string  (** relative to infer models *)
end

module Z = struct
  type t = int
  (** Dummy for Zarith module *)
end

module IntLit = struct
  type signedness = Signed | Unsigned

  type pointerness = NotPointer | Pointer

  type t = {signedness: signedness; i: Z.t; pointerness: pointerness}

  exception OversizedShift
end

module QualifiedCppName = struct
  exception ParseError of string
  (** internally it uses reversed list to store qualified name, for
     example: ["get", "shared_ptr<int>", "std"] *)

  type t = string list
end

module Mangled = struct
  type t = {plain: string; mangled: string option}
  (** for mangled names *)
end

module Annot = struct
  type parameters = string list

  type t =
    { class_name: string  (** name of the annotation *)
    ; parameters: parameters  (** currently, only one string parameter *) }
  (** Represent one @annotation *)

  module Item = struct
    (* Don't use nonrec due to https://github.com/janestreet/ppx_compare/issues/2 *)
    (* type nonrec t = list (t, bool) [@@deriving compare]; *)
    (** Annotation for one item: a list of annotations with visibility. *)

    type t_ = (t * bool) list

    type t = t_
  end

  module Class = struct end

  module Method = struct
    type t = {return: Item.t; parames: Item.t list}
  end
end

module Typ = struct
  (** The Smallfoot Intermediate Language: Types *)

  module IntegerWidths = struct
    type t =
      { char_width: int
      ; short_width: int
      ; int_width: int
      ; long_width: int
      ; longlong_width: int }
  end

  (** Kinds of integers *)
  type ikind =
    | IChar
    | ISChar  (** signed char *)
    | IUChar  (** unsigned char *)
    | IBool
    | IInt
    | IUInt
    | IShort
    | IUShort
    | ILong
    | IULong
    | ILongLong
    | IULongLong
    | I128
    | IU128

  (** Kinds of floating-point numbers *)
  type fkind = FFloat | FDouble | FLongDouble

  (** Kind of pointers *)
  type ptr_kind =
    | Pk_pointer  (** C/C++, Java, Objc standard / __strong pointer *)
    | Pk_reference  (** C++ reference *)
    | Pk_objc_weak  (** Obj-C __weak pointer *)
    | Pk_objc_unsafe_unretained  (** Obj-C __unsafe_unretained pointer *)
    | Pk_objc_autoreleasing  (** Obj-C __autoreleasing pointer *)

  module T = struct
    type type_quals = {is_const: bool; is_restrict: bool; is_volatile: bool}

    type t = {desc: desc; quals: type_quals}
    (** types for sil (structured) expresions *)

    and desc =
      | Tint of ikind
      | Tfloat of fkind
      | Tvoid
      | Tfun of {no_return: bool}
      | Tptr of t * ptr_kind
      | Tstruct of name
      | TVar of string  (** type variable (ie. C++ template variables *)
      | Tarray of {elt: t; length: IntLit.t option; stride: IntLit.t option}
          (** array type with statically fixed length and stride *)

    and name =
      | CStruct of QualifiedCppName.t
      | CUnion of QualifiedCppName.t
      | CppClass of QualifiedCppName.t * template_spec_info
      | JavaClass of Mangled.t
      | ObjcClass of QualifiedCppName.t
      | ObjcProtocol of QualifiedCppName.t

    and template_arg =
      | TType of t
      | TInt of Int64.t
      | TNull
      | TNullPtr
      | TOpaque

    and template_spec_info =
      | NoTemplate
      | Template of {mangled: string option; args: template_arg list}
  end

  include T

  module Name = struct
    type t = name
    (** T.name *)

    module C = struct
      (** ... some values *)
    end

    module Java = struct
      module Split = struct
        (** e.g. {type_name="int"; package=None} for primitive types
           or {type_name="PrintWriter"; package=Some "java.io"} for
           objects *)

        type t = {packaeg: string option; type_name: string}
      end
    end

    module Cpp = struct
      (** ... some values *)
    end

    module Objc = struct
      (** ... some values *)
    end
  end

  type typ = t

  module Procname = struct
    (** Level of verbosity of some to_string functions *)
    type detail_level = Verbose | Non_verbose | Simple

    module Java = struct
      type kind =
        | Non_Static
            (** in Java, procedures called with invokevirtual,
           invokespecial, and invokeinterface *)
        | Static  (** in Java, procedures called with invokestatic *)

      type java_type = Name.Java.Split.t

      type t =
        { method_name: string
        ; parameters: java_type list
        ; class_name: Name.t
        ; return_type: java_type option
        ; kind: kind }
    end

    module Parameter = struct
      type clang_parameter = Name.t option
      (** Type for parameters in clang procnams, [Some name] means the
         parameter is of type pointer to struct, with [name] being the
         name of the struct, [None] means the parameter is of some
         other type. *)

      type t =
        | JavaParameter of Java.java_type
        | ClangParameter of clang_parameter
    end

    module ObjC_Cpp = struct
      type kind =
        | CPPMethod of {mangled: string option}
        | CPPConstructor of {mangled: string option; is_constexpr: bool}
        | CPPDestructor of {mangled: string option}
        | ObjCClassMethod
        | ObjCInstanceMethod
        | ObjCInternalMethod

      type t =
        { class_name: Name.t
        ; kind: kind
        ; method_name: string
        ; parameters: Parameter.clang_parameter list
        ; template_args: template_spec_info }
    end

    module C = struct
      (** Type of C procedure names *)

      type t =
        { name: QualifiedCppName.t
        ; mangled: string option
        ; parameters: Parameter.clang_parameter list
        ; template_args: template_spec_info }
    end

    module Block = struct
      (** Type of Object C block names *)

      type block_name = string

      type t = {name: block_name; parameters: Parameter.clang_parameter list}
    end

    (** Type of procedure names *)
    type t =
      | Java of Java.t
      | C of C.t
      | Linters_dummy_method
      | Block of Block.t
      | ObjC_Cpp of ObjC_Cpp.t
      | WithBlockParameters of t * Block.block_name list
  end

  module Fieldname = struct
    type t =
      | Clang of {class_name: Name.t; field_name: string}
      | Java of string
  end

  module Struct = struct
    type field = Fieldname.t * T.t * Annot.Item.t

    type fields = field list

    type t =
      { fields: fields  (** non-static fields *)
      ; statics: fields  (** static fields *)
      ; supers: Name.t list  (** superclasses *)
      ; methods: Procname.t list  (** methods defined *)
      ; exported_objc_methods: Procname.t list
      ; annots: Annot.Item.t  (** annotations *) }
    (** Type for a structured value *)

    type lookup = Name.t -> t option
  end
end

module Location = struct
  (** Location in the original source file *)

  type t =
    { line: int  (** line number. -1 menas "do not know" *)
    ; col: int
    ; file: SourceFile.t }
end

module Pvar = struct
  (** The Smallfoot Intermediate Language *)

  type translation_unit = SourceFile.t option

  (** Kind of global variables *)
  type pvar_kind =
    | Local_var of Typ.Procname.t
        (** local variable belonging to a function *)
    | Callee_var of Typ.Procname.t  (** local variable belonging to a callee *)
    | Abduced_retvar of Typ.Procname.t * Location.t
        (** synthetic variable to represent return value *)
    | Abduced_ref_param of Typ.Procname.t * int * Location.t
        (** synthetic variable to represent param passed by reference *)
    | Global_var of
        { translation_unit: translation_unit
        ; is_constexpr: bool (* is it compile constant? *)
        ; is_ice: bool (* is it integral constant expression? *)
        ; is_pod: bool
        ; is_static_local: bool
        ; is_static_global: bool }
    | Seed_var
        (** variable used to store the initial value of formal parameters *)

  type t = {pv_hash: int; pv_name: Mangled.t; pv_kind: pvar_kind}
  (** Names for program variables *)
end

module Var = struct
  (** Single abstraction for all the kinds of variables in SIL *)

  type t = LogicalVar of Ident.t | ProgramVar of Pvar.t
end

module AccessPath = struct
  module Raw = struct
    type typ_ = Typ.t

    (* ignores types while comparing bases. we cannot trust the types
       from all of our frontends to be consistent, and the variable
       names should already be enough to distinguish the bases. *)
  end
end
