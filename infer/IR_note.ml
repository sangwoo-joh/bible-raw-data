open Base_note
open Istd_note

(*********************** Name related ***************************)
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

(************************************* Type related *****************************)
module Typ = struct
  (** The Smallfoot Intermediate Language: Types *)

  module IntegerWidths = struct
    type t =
      {char_width: int; short_width: int; int_width: int; long_width: int; longlong_width: int}
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

    and template_arg = TType of t | TInt of Int64.t | TNull | TNullPtr | TOpaque

    and template_spec_info =
      | NoTemplate
      | Template of {mangled: string option; args: template_arg list}
  end

  include T

  module Name = struct
    type t = name
    (** T.name *)

    let equal : t -> t -> bool = failwith "dummy"

    let hash = Hashtbl.hash

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

      type t = JavaParameter of Java.java_type | ClangParameter of clang_parameter
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

    let equal : t -> t -> bool = failwith "dummy"

    let compare : t -> t -> int = failwith "dummy"

    let hash = Hashtbl.hash

    module Hashable = struct
      type nonrec t = t

      let equal = equal

      let hash = hash
    end

    module Hash = Hashtbl.Make (Hashable)

    module Set = Set.Make (struct
      type nonrec t = t

      let compare = compare
    end)
  end

  module Fieldname = struct
    type t = Clang of {class_name: Name.t; field_name: string} | Java of string
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

module Tenv = struct
  module TypenameHash = Caml.Hashtbl.Make (Typ.Name)
  module TypenameHashNormalizer = MaximumSharing.ForHashtbl (TypenameHash)

  type t = Typ.Struct.t TypenameHash.t

  type per_file = Global | FileLocal of t
end

module Subtype = struct
  type t' = Exact  (** denotes the current type only *) | Subtypes of Typ.Name.t list

  (** Denotes the current type and a list of types that are not their subtypes *)
  type kind = CAST | INSTOF | NORMAL

  type t = t' * kind

  type result = No | Unknown | Yes
end

module Pvar = struct
  (** The Smallfoot Intermediate Language *)

  type translation_unit = SourceFile.t option

  (** Kind of global variables *)
  type pvar_kind =
    | Local_var of Typ.Procname.t  (** local variable belonging to a function *)
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
    | Seed_var  (** variable used to store the initial value of formal parameters *)

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
    type base = Var.t * typ_

    type access = ArrayAccess of typ_ * t list | FieldAccess of Typ.Fieldname.t

    and t = base * access list
  end

  module Abs = struct
    type raw = Raw.t

    type t = Abstracted of Raw.t | Exact of Raw.t
  end

  include Raw

  module BaseMap = Caml.Map.Make (struct
    type t = base

    let compare : t -> t -> int = failwith "dummy"
  end)
end

(********************************** Actual IR related *****************************)

module Unop = struct
  type t = Neg | BNot  (** Bitwise complement ~ *) | LNot  (** Logical Not ! *)
end

module Binop = struct
  type ikind_option_for_binop = Typ.ikind option

  type t =
    | PlusA of ikind_option_for_binop  (** arithmetic + *)
    | PlusPI  (** pointer + integer *)
    | MinusA of ikind_option_for_binop  (** arithmetic - *)
    | MinusPI  (** pointer - integer *)
    | MinusPP  (** pointer - pointer *)
    | Div
    | Mod
    | Shiftlt
    | Shiftrt
    | Lt
    | Gt
    | Le
    | Ge
    | Eq
    | Ne
    | BAnd
    | BXor
    | BOr
    | LAnd
    | LOr
end

module Const = struct
  type t =
    | Cint of IntLit.t
    | Cfun of Typ.Procname.t  (** function names *)
    | Cstr of string
    | Cfloat of float
    | Cclass of Ident.name
end

module Exp = struct
  (* reverse the natural order on Var *)
  type ident_ = Ident.t

  type closure = {name: Typ.Procname.t; captured_vars: (t * Pvar.t * Typ.t) list}

  and sizeof_data = {typ: Typ.t; nbytes: int option; dynamic_length: t option; subtype: Subtype.t}
  (** This records information about a [sizeof(typ)] expression. *)

  (** Program expressions *)
  and t =
    | Var of ident_  (** Pure variable: it is not an lvalue *)
    | UnOp of Unop.t * t * Typ.t option  (** Unary operator with type of the result if known *)
    | BinOp of Binop.t * t * t
    | Exn of t  (** Exception *)
    | Closure of closure  (** Anonymous function *)
    | Const of Const.t
    | Cast of Typ.t
    | Lvar of Pvar.t  (** The address of a program variable *)
    | Lfield of t * Typ.Fieldname.t * Typ.t
        (** A field offset, the type is the surrounding struct type *)
    | Lindex of t * t  (** An array index offset: [exp1[exp2]] *)
    | Sizeof of sizeof_data
end

module CallFlags = struct
  (** Flags for a procedure call *)

  type t =
    { cf_assign_last_arg: bool
    ; cf_injected_destructor: bool
    ; cf_interface: bool
    ; cf_is_objc_block: bool
    ; cf_noreturn: bool
    ; cf_virtual: bool
    ; cf_with_block_parameters: bool }
end

module HilExp = struct
  type typ_ = Typ.t

  module Access = struct
    type 'array_index t =
      | FieldAccess of Typ.Fieldname.t
      | ArrayAccess of typ_ * 'array_index
      | TakeAddress
      | Dereference
  end

  (** Module where unsafe construction of [access_expression] is allowed.
      In the rest of the code, and especially in clients of the whole [AccessExpression] module,
      we do not want to allow constructing access expressions directly as they could introduce
      de-normalized expressions of the form [AddressOf (Dereference t)] or [Dereference (AddressOf t)].
      We could make only the types of [AddressOf] and [Dereference] private but that proved too cumbersome...
  *)

  module T : sig
    type t =
      | AccessExpression of access_expression
      | UnaryOperator of Unop.t * t * Typ.t option
      | BinaryOperator of Binop.t * t * t
      | Exception of t
      | Closure of Typ.Procname.t * (AccessPath.base * t) list
      | Constant of Const.t
      | Cast of Typ.t * t
      | Sizeof of Typ.t * t option

    and access_expression = private
      | Base of AccessPath.base
      | FieldOffset of access_expression * Typ.Fieldname.t
      | ArrayOffset of access_expression * typ_ * t option
      | AddressOf of access_expression
      | Dereference of access_expression

    module UnsafeAccessExpression : sig
      val base : AccessPath.base -> access_expression

      val field_offset : access_expression -> Typ.Fieldname.t -> access_expression

      val array_offset : access_expression -> Typ.t -> t option -> access_expression

      val address_of : access_expression -> access_expression option

      val address_of_base : AccessPath.base -> access_expression

      val dereference : access_expression -> access_expression

      val replace_base :
        remove_deref_after_base:bool -> AccessPath.base -> access_expression -> access_expression
    end
  end = struct
    type t =
      | AccessExpression of access_expression
      | UnaryOperator of Unop.t * t * Typ.t option
      | BinaryOperator of Binop.t * t * t
      | Exception of t
      | Closure of Typ.Procname.t * (AccessPath.base * t) list
      | Constant of Const.t
      | Cast of Typ.t * t
      | Sizeof of Typ.t * t option

    and access_expression =
      | Base of AccessPath.base
      | FieldOffset of access_expression * Typ.Fieldname.t
      | ArrayOffset of access_expression * typ_ * t option
      | AddressOf of access_expression
      | Dereference of access_expression

    module UnsafeAccessExpression = struct
      let base base = Base base

      let field_offset t field = FieldOffset (t, field)

      let array_offset t typ index = ArrayOffset (t, typ, index)

      let address_of = function
        | Dereference _ | AddressOf _ ->
            None
        | (FieldOffset _ | ArrayOffset _ | Base _) as t ->
            Some (AddressOf t)

      let address_of_base base = AddressOf (Base base)

      let dereference = function AddressOf t -> t | t -> Dereference t

      let rec replace_base ~remove_deref_after_base base_new access_expr =
        let replace_base_inner = replace_base ~remove_deref_after_base base_new in
        match access_expr with
        | Dereference (Base _) ->
            if remove_deref_after_base then Base base_new else Dereference (Base base_new)
        | Base _ ->
            Base base_new
        | FieldOffset (ae, fld) ->
            FieldOffset (replace_base_inner ae, fld)
        | ArrayOffset (ae, typ, x) ->
            ArrayOffset (replace_base_inner ae, typ, x)
        | AddressOf ae ->
            AddressOf (replace_base_inner ae)
        | Dereference ae ->
            Dereference (replace_base_inner ae)
    end
  end

  include T

  (** Why is this module needed? *)
  module AccessExpression = struct
    include UnsafeAccessExpression

    type nonrec t = access_expression = private
      | Base of AccessPath.base
      | FieldOffset of access_expression * Typ.Fieldname.t
      | ArrayOffset of access_expression * typ_ * t option
      | AddressOf of access_expression
      | Dereference of access_expression
  end
end

module DecompiledExp = struct
  (** expression representing the result of decompilation *)
  type t =
    | Darray of t * t
    | Dbinop of Binop.t * t * t
    | Dconst of Const.t
    | Dsizeof of Typ.t * t option * Subtype.t
    | Dderef of t
    | Dfcall of t * t list * Location.t * CallFlags.t
    | Darrow of t * Typ.Fieldname.t
    | Ddot of t * Typ.Fieldname.t
    | Dpvar of Pvar.t
    | Dpvaraddr of Pvar.t
    | Dunop of Unop.t * t
    | Dunknown
    | Dretcall of t * t list * Location.t * CallFlags.t

  type vpath = t option
  (** Value paths: identify an occurrence of a value in a symbolic
     heap each expression represents a path, with Dpvar being the
     simplest one *)
end

module PredSymb = struct
  (** what is this?? *)
  type func_attribute = FA_sentinel of int * int

  type access = Default | Public | Private | Protected

  type mem_kind = Mmalloc | Mnew | Mnew_array | Mobjc

  type resource = Rmemory of mem_kind | Rfile | Rignore | Rlock

  type res_act_kind = Racquire | Rrelease

  type dangling_kind =
    | DAuninit  (** pointer is dangling because it is uninitialized *)
    | DAaddr_stack_var
        (** because it is the address of a stack variable which went out of scope *)
    | DAminusone

  (* pointer is -1 *)

  type path_pos = Typ.Procname.t * int
  (** position in a path: proc name, node id *)

  type res_action =
    { ra_kind: res_act_kind
    ; ra_res: resource
    ; ra_pname: Typ.Procname.t  (** name of the procedure used to acquire/release the resource *)
    ; ra_loc: Location.t
    ; ra_vpath: DecompiledExp.vpath }
  (** acquire / release action on a resource *)

  (** type aliases for components of t values that compare should ignore *)

  type annot_item_ = Annot.Item.t

  type location_ = Location.t

  type path_pos_ = path_pos

  (** Attributes are n-ary function symbols that are applied to
     expression arguments in Apred and Anpred atomic formulas.  Many
     operations don't make much sense for nullary predicates, and are
     generally treated as no-ops. The first argument is treaded
     specially, as the "anchor" of the predicate application. e.g.,
     adding or removing an attribute uses the anchor to identify the
     atom to operate on. Also, abstraction and normalization
     operations treat the anchor specially and maintain more
     information on it than other arguments. Therefore, when attaching
     an attribute to an expression, that expression should be the
     first argument, optionally followed by additional related
     expressions. *)
  type t =
    | Aresource of res_action
    | Aautorelease
    | Adangling of dangling_kind
    | Aundef of Typ.Procname.t * annot_item_ * location_ * path_pos_
    | Alocked
    | Aunlocked
    | Adiv0 of path_pos
    | Aobjc_null
    | Aretval of Typ.Procname.t * Annot.Item.t
    | Aobserver  (** denots an object registered as an observer to a notification center *)
    | Aunsubscribed_observer
    | Awont_leak  (** value do not participate in memory leak analysis *)

  type category =
    | ACresource
    | ACautorelease
    | AClock
    | ACdiv0
    | ACobjc_null
    | ACundef
    | ACretval
    | ACobserver
    | ACwontleak
end

module Sil = struct
  (** The Smallfoot Intermediate Language *)

  (** Kind of prune instruction *)
  type if_kind =
    | Ik_bexp  (** boolean expressions, and exp ? exp : exp *)
    | Ik_dowhile
    | Ik_for
    | Ik_if
    | Ik_land_lor
    | Ik_while
    | Ik_switch

  type instr_metadata =
    | Abstract of Location.t
        (** a good place to apply abstraction, mostly used in the biabduction analysis *)
    | ExitScope of Var.t list * Location.t  (** remove temporaries and dead program variables *)
    | Nullify of Pvar.t * Location.t  (** nullify stack variable *)
    | Skip
    | VariableLifetimeBegins of Pvar.t * Typ.t * Location.t  (** stack variable declared *)

  (** An instruction *)
  type instr =
    (* [x] must be used in a subsequent instruction, otherwise the
       entire `Load` instruction may be eliminated by copy-propagation
    *)
    | Load of Ident.t * Exp.t * Typ.t * Location.t
        (** Load a value from the heap into an identifier.
        [x = *lexp:typ] where
        - [lexp] is an expression denoting a heap address
        - [typ] is the root type of [lexp] *)
    | Store of Exp.t * Typ.t * Exp.t * Location.t
        (** Store the value of an expression into the heap.
        [*lexp1:typ = exp2] where
        - [lexp1] is an expression denoting a heap address
        - [typ] is the root type of [lexp1]
        - [exp2] is the expression whose value is stored *)
    | Prune of Exp.t * Location.t * bool * if_kind
        (** Prune the state based on [exp=1], the boolean indicates whether true branch *)
    | Call of (Ident.t * Typ.t) * Exp.t * (Exp.t * Typ.t) list * Location.t * CallFlags.t
        (** [Call ((ret_id, ret_typ), e_fun, arg_ts, loc, call_flags))] represents an instruction
        [ret_id = e_fun(arg_ts);] *)
    | Metadata of instr_metadata
        (** Hints about the program that are not strictly needed to
        understand its semantics, for instance information about its
        original syntactic structure *)

  (** offset for an lvalue *)
  type offset = Off_fld of Typ.Fieldname.t * Typ.t | Off_index of Exp.t

  (** An atom is a pure atomic formula *)
  type atom =
    | Aeq of Exp.t * Exp.t
    | Aneq of Exp.t * Exp.t
    | Apred of PredSymb.t * Exp.t list  (** predicate symbol applied to exps *)
    | Anpred of PredSymb.t * Exp.t list  (** negated predicate symbol applied to exps *)

  (** Kind of lseg or dllseg predicates ??*)
  type lseg_kind =
    | Lseg_NE  (** nonempty (possibly circular) listseg *)
    | Lseg_PE  (** possibly empty (possibly circular) listseg *)

  type zero_flag = bool option
  (** The boolean is true when the pointer was dereferenced without testing for zero *)

  type null_case_flag = bool
  (** True when the value was obtained by doing case analysis on null in a procedure call *)

  (** instrumentation of heap values *)
  type inst =
    | Iabstraction
    | Iactual_precondition
    | Ialloc
    | Iformal of zero_flag * null_case_flag
    | Iinitial
    | Ilookup
    | Inone
    | Inullify
    | Irearrange of zero_flag * null_case_flag * int * PredSymb.path_pos
    | Itaint
    | Iupdate of zero_flag * null_case_flag * int * PredSymb.path_pos
    | Ireturn_from_call of int

  (** structured expressions represent a value of structured type, such as an array or a struct *)
  type 'inst strexp0 =
    | Eexp of Exp.t * 'inst  (** Base case: expression with instrumentation *)
    | Estruct of (Typ.Fieldname.t * 'inst strexp0) list * 'inst  (** C structure *)
    | Earray of Exp.t * (Exp.t * 'inst strexp0) list * 'inst
        (** Array of given length.
        There are two conditions imposed / used in the array case.
        First, if some index and value pair appears inside an array in a strexp,
        then the index is less than the length of the array.
        e.g. x |-> [10 | e1: v1] implies e1 <= 9
        Second, if two indices appear in an array, they should be different.
        e.g. x |-> [10 | e1: v1, e2: v2] implies e1 != e2 *)

  type strexp = inst strexp0

  (** an atomic heap predicate *)
  type 'inst hpred0 =
    | Hpointsto of Exp.t * 'inst strexp0 * Exp.t
        (** represents [exp |-> strexp: typexp] where
        - [typexp] is an expression representing a type, e.g., [sizeof(t)] *)
    | Hlseg of lseg_kind * 'inst hpara0 * Exp.t * Exp.t * Exp.t list
        (** higher-order predicate for singly-linked lists.
        Should ensure that exp1 != exp2 implies that exp1 is allocated.
        This assumption is used in the rearrangement.
        The last [exp list] parameter is used to denote the shared links by all the nodes in the list. *)
    | Hdllseg of lseg_kind * 'inst hpara_dll0 * Exp.t * Exp.t * Exp.t * Exp.t * Exp.t list
        (** higher-order predicate for doubly-linked lists.
        Parameter for the higher-order singly-linked list predicate.
        Means "lambda (root,next,svars). Exists evars. body".
        Assume that root, next, svars, evars are disjoint sets of
        primed identifiers, and include all the free primed identifiers in body.
        body should not contain any non - primed identifiers or program variables (i.e. pvars). *)

  and 'inst hpara0 =
    { root: Ident.t
    ; next: Ident.t
    ; svars: Ident.t list
    ; evars: Ident.t list
    ; body: 'inst hpred0 list }

  and 'inst hpara_dll0 =
    { cell: Ident.t  (** address cell *)
    ; blink: Ident.t  (** backward *)
    ; flink: Ident.t  (** forward *)
    ; svars_dll: Ident.t list
    ; evars_dll: Ident.t list
    ; body_dll: 'inst hpred0 list }
  (** parameter for the higher-order doubly-linked list predicates.
      Assume that all the free identifiers in body_dll should belong to
      cell, blink, flink, svars_dll, evars_dll. *)

  type hpred = inst hpred0

  type hpara = inst hpara0

  type hpara_dll = inst hpara_dll0

  type ident_dexp = Ident.t * Exp.t

  type subst = ident_dexp list

  type subst_fun = Ident.t -> Exp.t
end

module CallSite = struct
  type t = {pname: Typ.Procname.t; loc: Location.t}
end

(************************************* CFG related ****************************)

module HilInstr = struct
  type call = Direct of Typ.Procname.t | Indirect of HilExp.AccessExpression.t

  type t =
    | Assign of HilExp.AccessExpression.t * HilExp.t * Location.t
    | Assume of HilExp.t * [`Then | `Else] * Sil.if_kind * Location.t
    | Call of AccessPath.base * call * HilExp.t list * CallFlags.t * Location.t
    | Metadata of Sil.instr_metadata

  type translation = Instr of t | Bind of Var.t * HilExp.AccessExpression.t
end

module Instrs = struct
  (* Some functions are only used on non-reversed arrays, let's specialize them.
  The argument of the type helps us make sure they can't be used otherwise. *)
  module RevArray : sig
    type 'a t

    val is_empty : 'a t -> bool

    val length : 'a t -> int

    val of_rev_array : 'a Base.Array.t -> 'a t

    val get : 'a t -> int -> 'a

    val last_opt : 'a t -> 'a option

    val fold : ('a t, 'a, 'accum) Base.Container.fold
  end = struct
    type 'a t = 'a Base.Array.t

    let is_empty = Base.Array.is_empty

    let length = Base.Array.length

    let of_rev_array a = a

    let get a index = a.(Base.Array.length a - 1 - index)

    let last_opt a = if is_empty a then None else Some (Base.Array.unsafe_get a 0)

    let fold a ~init ~f =
      let f = Base.Fn.flip f in
      Base.Array.fold_right a ~init ~f
  end

  type reversed

  type not_reversed

  type 'rev t =
    | NotReversed : Sil.instr Base.Array.t -> not_reversed t
    | Reversed : Sil.instr RevArray.t -> reversed t

  type not_reversed_t = not_reversed t
end

module ClangMethodKind = struct
  type t = CPP_INSTANCE | OBJC_INSTANCE | CPP_CLASS | OBJC_CLASS | BLOCK | C_FUNCTION
end

module ProcAttributes = struct
  type objc_accessor_type = Objc_getter of Typ.Struct.field | Objc_setter of Typ.Struct.field

  type var_data = {name: Mangled.t; typ: Typ.t; modify_in_block: bool; is_constexpr: bool}

  type t =
    { access: PredSymb.access  (** visibility access *)
    ; captured: (Mangled.t * Typ.t) list  (** name and type of variables captured in blocks *)
    ; exceptions: string list  (** exceptions thrown by the procedure *)
    ; formals: (Mangled.t * Typ.t) list  (** name and type of formal parameters *)
    ; const_formals: int list  (** list of indices of formals that are const-qualified *)
    ; func_attributes: PredSymb.func_attribute list
    ; is_abstract: bool  (** the procedure is abstract *)
    ; is_biabduction_model: bool  (** the procedure is a model for the biabduction analysis *)
    ; is_bridge_method: bool  (** the procedure is a bridge method *)
    ; is_defined: bool  (** true if the procedure is defined, and not just declared *)
    ; is_cpp_noexcept_method: bool  (** the procedure is an C++ method annotated with "noexcept" *)
    ; is_java_synchronized_method: bool  (** the procedure is a Java synchronized method *)
    ; is_specialized: bool
          (** the procedure is a clone specialized for dynamic dispatch handling *)
    ; is_synthetic_method: bool  (** the procedure is a synthetic method *)
    ; is_variadic: bool  (** the procedure is variadic, only supported for Clang procedures *)
    ; clang_method_kind: ClangMethodKind.t  (** the kind of method the procedure is *)
    ; loc: Location.t  (** location of this procedure in the source code *)
    ; translation_unit: SourceFile.t  (** translation unit to which the procedure belongs *)
    ; mutable locals: var_data list  (** name, type and attributes of local variables *)
    ; method_annotation: Annot.Method.t  (** annotations for all methods *)
    ; objc_accessor: objc_accessor_type option  (** type of ObjC accessor, if any *)
    ; proc_name: Typ.Procname.t  (** name of the procedure *)
    ; ret_type: Typ.t  (** return type *)
    ; has_added_return_param: bool  (** whether or not a return param was added *) }
end

module WeakTopologicalOrder = struct
  module Partition = struct
    type 'node t =
      | Empty
      | Node of {node: 'node; next: 'node t}
      | Component of {head: 'node; rest: 'node t; next: 'node t}
  end

  module type PreProcCfg = sig
    module Node : sig
      type t

      type id

      val id : t -> id

      module IdMap : Map.S with type key = id
      (** ppmap in real code *)
    end

    type t

    val fold_succs : t -> (Node.t, Node.t, 'accum) Base.Container.fold

    val start_node : t -> Node.t
  end

  module type S = sig
    module CFG : PreProcCfg

    val make : CFG.t -> CFG.Node.t Partition.t
  end

  module type Make = functor (CFG : PreProcCfg) -> S with module CFG = CFG

  module Bourdoncle_SCC (CFG : PreProcCfg) = struct
    module CFG = CFG

    module Dfn = CFG.Node.IdMap
    (** [dfn] contains a DFS pre-order indexing. A node is not in the
       map if it has never been visited. A node's dfn is +oo if it has
       been fully visited (head of cross-edges) or we want to hide it
       for building a subcomponent partition (head of highest
       back-edges).  *)

    (* Unlike Bourdoncle's paper version or OCamlGraph implementation,
       this implementation handles high DFS-depth graphs, which would
       stack-overflow otherwise. It still doesn't handle high
       component nesting, but it is pretty unlikely to happen in real
       code (means a lot of loop nesting).  *)

    type stack =
      { node: CFG.Node.t
      ; node_id: CFG.Node.id
      ; node_dfn: int
      ; succs: CFG.Node.t list
      ; mutable succs_to_visit: CFG.Node.t list
      ; mutable head: int  (** Minimum [dfn] of the nodes accessibles from [node]. *)
      ; mutable component: CFG.Node.id ARList.t
            (** Nodes in the current strict-connected component. *)
      ; mutable building_component: bool
      ; next: stack option }
  end
end

module Procdesc = struct
  module NodeKey = struct
    type t = Digest.t
  end

  module Node = struct
    type id = int

    type destruction_kind =
      | DestrBreakStmt
      | DestrContinueStmt
      | DestrFields
      | DestrReturnStmt
      | DestrScope
      | DestrTemporariesCleanup
      | DestrVirtualBase

    type stmt_nodekind =
      | AssertionFailure
      | BetweenJoinAndExit
      | BinaryConditionalStmtInit
      | BinaryOperatorStmt of string
      | Call of string
      | CallObjCNew
      | CallRetBinding
      | ClassCastException
      | ConditionalStmtBranch
      | ConstructorInit
      | CXXDynamicCast
      | CXXNewExpr
      | CXXStdInitializerListExpr
      | CXXTypeidExpr
      | DeclStmt
      | DefineBody
      | Destruction of destruction_kind
      | ExceptionHandler
      | ExceptionsSink
      | ExprWithCleanups
      | FallbackNode
      | FinallyBranch
      | GCCAsmStmt
      | GenericSelectionExpr
      | IfStmtBranch
      | InitializeDynamicArrayLength
      | InitListExp
      | MessageCall of string
      | MethodBody
      | MonitorEnter
      | MonitorExit
      | ObjCCPPThrow
      | OutOfBound
      | ReturnStmt
      | Scope of string
      | Skip of string
      | SwitchStmt
      | ThisNotNull
      | Throw
      | ThrowNPE
      | UnaryOperator

    type prune_node_kind =
      | PruneNodeKind_ExceptionHandler
      | PruneNodeKind_FalseBranch
      | PruneNodeKind_InBound
      | PruneNodeKind_IsInstance
      | PruneNodeKind_MethodBody
      | PruneNodeKind_NotNull
      | PruneNodeKind_TrueBranch

    type nodekind =
      | Start_node
      | Exit_node
      | Stmt_node of stmt_nodekind
      | Join_node
      | Prune_node of bool * Sil.if_kind * prune_node_kind
          (** (true/false branch, if_kind, comment) *)
      | Skip_node of string

    type t =
      { id: id  (** uuid *)
      ; mutable dist_exit: int option  (** distance to the exit node *)
      ; mutable wto_index: int
      ; mutable exn: t list  (** exception nodes in the cfg *)
      ; mutable instrs: Instrs.not_reversed_t  (** instructions for symbolic execution *)
      ; kind: nodekind
      ; loc: Location.t
      ; mutable preds: t list  (** predecessors *)
      ; pname: Typ.Procname.t  (** name of the procedure the node belongs to *)
      ; mutable succs: t list  (** successors *) }
    (** a node *)

    let compare : t -> t -> int = failwith "dummy"

    type node = t

    module NodeSet = Set.Make (struct
      type t = node

      let compare = compare
    end)
  end

  module NodeSet = Node.NodeSet

  type t =
    { mutable attributes: ProcAttributes.t  (** attributes of the procedure *)
    ; mutable nodes: Node.t list
    ; mutable nodes_num: int
    ; mutable start_node: Node.t
    ; mutable exit_node: Node.t
    ; mutable loop_heads: NodeSet.t option  (** loop head nodes of this procedure *)
    ; mutable wto: Node.t WeakTopologicalOrder.Partition.t option }
  (** Procedure Description *)
end

module Cfg = struct
  type t = Procdesc.t Typ.Procname.Hash.t
end

(***************************************** Related to Errlog *********************)
module IssueType = struct
  module Unsafe : sig
    type t = private
      { unique_id: string
      ; mutable enabled: bool
      ; mutable hum: string
      ; mutable doc_url: string option
      ; mutable linters_def_file: string option }
  end = struct
    module T = struct
      type t =
        { unique_id: string
        ; mutable enabled: bool
        ; mutable hum: string
        ; mutable doc_url: string option
        ; mutable linters_def_file: string option }

      let compare {unique_id= id1} {unique_id= id2} = String.compare id1 id2

      let equal = failwith "dummy"
    end

    include T
    module IssueSet = Caml.Set.Make (T)
  end

  include Unsafe
end

module Localise = struct
  module Tags = struct
    type t = (string * string) list
  end

  type error_desc = {descriptions: string list; tags: Tags.t; dotty: string option}

  type deref_str =
    { tags: (string * string) list ref  (** tags for the error description *)
    ; value_pre: string option  (** string printed before the value being dereferenced *)
    ; value_post: string option  (** string printed after the value being dereferenced *)
    ; problem_str: string  (** description of the problem *) }

  type access =
    | Last_assigned of int * bool
    (* line, null_case_flag *)
    | Last_accessed of int * bool
    (* line, is_nullable flag *)
    | Initialized_automatically
    | Returned_from_call of int

  (** kind of precondition not met *)
  type pnm_kind = Pnm_bounds | Pnm_dangling
end

type ocaml_pos = string * int * int * int

module Exceptions = struct
  type visibility =
    | Exn_user  (** always add to error log *)
    | Exn_developer  (** only add to error log in developer mode *)
    | Exn_system  (** never add to error log *)

  (** class of error/warning *)
  type err_class = Checker | Prover | Nocat | Linters

  (** class of error/warning *)
  type severity = Like | Info | Advice | Warning | Error

  type t =
    { name: IssueType.t
    ; description: Localise.error_desc
    ; ocaml_pos: ocaml_pos option  (** location in the infer source code *)
    ; visibility: visibility
    ; severity: severity option
    ; category: err_class }
end

module Errlog = struct
  type node_tag =
    | Condition of bool
    | Exception of Typ.name
    | Procedure_start of Typ.Procname.t
    | Procedure_end of Typ.Procname.t

  type loc_trace_elem =
    { lt_level: int  (** nesting level of procedure calls *)
    ; lt_loc: Location.t  (** source location at the current step in the trace *)
    ; lt_description: string
    ; lt_node_tags: node_tag list }

  type loc_trace = loc_trace_elem list

  type node =
    | UnknownNode
    | FrontendNode of {node_key: Procdesc.NodeKey.t}
    | BackendNode of {node: Procdesc.Node.t}

  type err_key =
    {severity: Exceptions.severity; err_name: IssueType.t; err_desc: Localise.error_desc}

  type err_data =
    { node_id: int
    ; node_key: Procdesc.NodeKey.t option
    ; session: int
    ; loc: Location.t
    ; loc_in_ml_source: ocaml_pos option
    ; loc_trace: loc_trace
    ; err_class: Exceptions.err_class
    ; visibility: Exceptions.visibility
    ; linters_def_file: string option
    ; doc_url: string option
    ; access: string option
    ; extras: string option (* NOTE: Please consider adding new fields as part of extras *) }

  module ErrDataSet = Caml.Set.Make (struct
    type t = err_data

    let compare : t -> t -> int = failwith "dummy; only compare loc"
  end)

  module ErrLogHash = struct
    module Key = struct
      type t = err_key

      let hash key = Hashtbl.hash (key.severity, key.err_name)

      let equal : t -> t -> bool = failwith "dummy"
    end

    include Hashtbl.Make (Key)
  end

  type t = ErrDataSet.t ErrLogHash.t
end
