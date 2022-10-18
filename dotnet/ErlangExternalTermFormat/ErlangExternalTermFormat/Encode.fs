module EETF.Encode

open System

open EETF.Type
open System.Buffers.Binary

// Append operator for arrays of bytes
let inline private (@) (x: byte[]) (y: byte[]) = Array.append x y

/// Encodes an F# float into 8 bytes in big-endian IEEE format.
/// See https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#float_ext
let private encodeFloat f =
    let array = Array.zeroCreate<byte>(8)
    let span = new Span<byte>(array)
    BinaryPrimitives.WriteDoubleBigEndian(span, f)
    span.ToArray()

/// Encodes an F# int (i.e., int32) as a 32-bit integer in big-endian format.
/// See https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#integer_ext
let private encodeInteger integer =
    let array = Array.zeroCreate<byte>(4)
    let span = new Span<byte>(array)
    BinaryPrimitives.WriteInt32BigEndian(span, integer)
    span.ToArray()

/// Encode an Erlang term value into an array of bytes according to the
/// Erlang external term format.
/// See https://www.erlang.org/doc/apps/erts/erl_ext_dist.html
let encode (term: Erlang) =
    match term with
    | Erlang.Integer i    -> if 0 <= i && i <= 255 then
                                 [| Tag.SmallIntegerExt; byte i|]
                             else
                                 [| Tag.IntegerExt |] @ (encodeInteger i)
    | Erlang.Float f      -> [| Tag.NewFloatExt |] @ (encodeFloat f)
    | Erlang.Tuple t      -> [| |]
    | Erlang.Nil          -> [| Tag.Version; Tag.NilExt |]
    | Erlang.Binary b     -> [| Tag.BinaryExt |] @ (encodeInteger (Array.length b)) @ b
    | Erlang.List l       -> [||]
    | Erlang.BigInteger i -> [||]
    | Erlang.Atom s       -> [||]
    | Erlang.Map m        -> [||]
    |> Array.append [| Tag.Version |]
