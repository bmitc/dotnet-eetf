/// Collection of byte tags for the supported Erlang term formats
[<RequireQualifiedAccess>]
module EETF.Tag

(*
This modulue gives a list of all supported tags. The terms and tags
are defined here: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html

The following terms/tags are supported:
    SMALL_INTEGER_EXT
    INTEGER_EXT
    FLOAT_EXT
    SMALL_TUPLE_EXT
    LARGE_TUPLE_EXT
    MAP_EXT
    NIL_EXT
    STRING_EXT
    LIST_EXT
    BINARY_EXT
    SMALL_BIG_EXT
    LARGE_BIG_EXT
    NEW_FLOAT_EXT
    ATOM_UTF8_EXT
    SMALL_ATOM_UTF8_EXT
    ATOM_EXT
    SMALL_ATOM_EXT

The following terms/tags are not supported:
    ATOM_CACHE_REF
    PORT_EXT
    NEW_PORT_EXT
    V4_PORT_EXT
    PID_EXT
    NEW_PID_EXT
    REFERENCE_EXT
    NEW_REFERENCE_EXT
    NEWER_REFERENCE_EXT
    FUN_EXT
    NEW_FUN_EXT
    EXPORT_EXT
    BIT_BINARY_EXT
*)

[<Literal>]
let Version = 131uy

[<Literal>]
let SmallIntegerExt = 97uy

[<Literal>]
let IntegerExt = 98uy

[<Literal>]
let FloatExt = 99uy

[<Literal>]
let SmallTupleExt = 104uy

[<Literal>]
let LargeTupleExt = 105uy

[<Literal>]
let MapExt = 116uy

[<Literal>]
let NilExt = 106uy

[<Literal>]
let StringExt = 107uy

[<Literal>]
let ListExt = 108uy 

[<Literal>]
let BinaryExt = 109uy

[<Literal>]
let SmallBigExt = 110uy

[<Literal>]
let LargeBigExt = 111uy

[<Literal>]
let NewFloatExt = 70uy

[<Literal>]
let AtomUTF8Ext = 118uy

[<Literal>]
let SmallAtomUTF8Ext = 119uy

[<Literal>]
let AtomExt = 100uy

[<Literal>]
let SmallAtomExt = 115uy
