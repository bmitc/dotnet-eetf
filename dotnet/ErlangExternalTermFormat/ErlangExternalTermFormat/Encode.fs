/// Functions for encoding F# Erlang type values as Erlang terms
module EETF.Encode

open System
open System.Buffers.Binary
open System.Numerics

open EETF.Type

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
let private encode32BitInteger integer =
    let array = Array.zeroCreate<byte>(4)
    let span = new Span<byte>(array)
    BinaryPrimitives.WriteInt32BigEndian(span, integer)
    span.ToArray()

/// Encodes an F# int (i.e., int32) as a 16-bit integer in big-endian format
let private encode16BitInteger integer =
    let array = Array.zeroCreate<byte>(2)
    let span = new Span<byte>(array)
    BinaryPrimitives.WriteInt16BigEndian(span, int16 integer)
    span.ToArray()

/// Determines if the given integer is a byte, i.e., in the range [0, 255]
let isByte integer = 0 <= integer && integer <= 255

/// Determines if the given integer is 16-bit integer, i.e., in the range [0, 65535]
let is16Bit integer = 0 <= integer && integer <= 65535

let isAllSmallIntegers (terms: Erlang list) =
    let isSmallInteger term =
        match term with
        | Erlang.Integer i when isByte i -> true
        | _                              -> false
    List.forall isSmallInteger terms

/// Encode an Erlang term value into an array of bytes according to the
/// Erlang external term format.
/// See https://www.erlang.org/doc/apps/erts/erl_ext_dist.html
let rec encode (term: Erlang) =
    match term with
    | Erlang.Integer i    -> if isByte i then
                                 [| Tag.SmallIntegerExt; byte i|]
                             else
                                 [| Tag.IntegerExt |] @ (encode32BitInteger i)

    | Erlang.Float f      -> [| Tag.NewFloatExt |] @ (encodeFloat f)

    | Erlang.Tuple t      -> let length = List.length t
                             let elements = List.toArray t
                                            |> Array.map encode
                                            |> Array.concat
                             if isByte length then
                                 [| Tag.SmallTupleExt; byte length |] @ elements
                             else
                                 [| Tag.LargeTupleExt |] @ (encode32BitInteger length) @ elements

    | Erlang.Nil          -> [| Tag.NilExt |]

    | Erlang.Binary b     -> [| Tag.BinaryExt |] @ (encode32BitInteger (Array.length b)) @ b

    | Erlang.List l       -> let length = List.length l
                             let allSmallIntegers = isAllSmallIntegers l
                             let elements = List.toArray l
                                            |> Array.map encode
                                            |> Array.concat
                             if allSmallIntegers && (is16Bit length) then
                                 [| Tag.StringExt |] @ (encode16BitInteger length) @ elements
                             else
                                 [| Tag.ListExt |] @ (encode32BitInteger length) @ elements @ [| Tag.NilExt |]

    | Erlang.BigInteger i -> let sign = if BigInteger.IsPositive i then 0uy else 1uy
                             let bytes = BigInteger.Abs(i).ToByteArray(isUnsigned = true, isBigEndian = false)
                             let length = Array.length bytes
                             if isByte length then
                                 [| Tag.SmallBigExt; byte length; sign |] @ bytes
                             else
                                 [| Tag.LargeBigExt |] @ (encode32BitInteger length) @ [| sign |] @ bytes

    | Erlang.Atom s       -> let atomName = Text.Encoding.UTF8.GetBytes(s)
                             let length = Array.length atomName
                             if isByte length then
                                 [| Tag.SmallAtomUTF8Ext; byte length |] @ atomName
                             else
                                 [| Tag.AtomUTF8Ext |] @ (encode16BitInteger length) @ atomName

    | Erlang.Map pairs    -> let numberOfPairs = List.length pairs
                             let arity = encode32BitInteger numberOfPairs
                             let pairs = List.toArray pairs
                                         |> Array.map (fun (key, value) -> Array.concat [|encode key; encode value|])
                                         |> Array.concat
                             [|Tag.MapExt|] @ pairs
    |> Array.append [| Tag.Version |]
