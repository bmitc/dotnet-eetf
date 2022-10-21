/// Functions for encoding F# Erlang type values as Erlang terms
module EETF.Encode

open System
open System.Buffers.Binary
open System.IO
open System.Numerics

open EETF.Type

(*
The Erlang external term format is of the form:

|-----------------|-----|-----|------|
| Number of Bytes |  1  |  1  |  N   |
|-----------------|-----|-----|------|
| Byte data       | 131 | Tag | Data |
|-----------------|-----|-----|------|

The version tag is only listed at the beginning of a term, and the Data bytes
can contain several types, such as the case for maps, tuples, and lists, and
so the Data bytes will also have Tag data for each instance of a type in Data.

The Erlang external term format is defined here:
https://www.erlang.org/doc/apps/erts/erl_ext_dist.html
*)

/// Append operator for arrays of bytes
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
let private isByte integer = 0 <= integer && integer <= 255

/// Determines if the given integer is 16-bit integer, i.e., in the range [0, 65535]
let private is16Bit integer = 0 <= integer && integer <= 65535

let private isAllSmallIntegers (terms: ErlangTerm list) =
    let isSmallInteger term =
        match term with
        | ErlangTerm.Integer i when isByte i -> true
        | _                              -> false
    List.forall isSmallInteger terms

/// Encode an Erlang term value into an array of bytes according to the
/// Erlang external term format.
/// See https://www.erlang.org/doc/apps/erts/erl_ext_dist.html
let rec private oldEncode (term: ErlangTerm) =
    match term with
    | Integer i    -> if isByte i then
                          [| Tag.SmallIntegerExt; byte i|]
                      else
                          [| Tag.IntegerExt |] @ (encode32BitInteger i)

    | Float f      -> [| Tag.NewFloatExt |] @ (encodeFloat f)

    | Tuple t      -> let length = List.length t
                      let elements = List.toArray t
                                     |> Array.map oldEncode
                                     |> Array.concat
                      if isByte length then
                          [| Tag.SmallTupleExt; byte length |] @ elements
                      else
                          [| Tag.LargeTupleExt |] @ (encode32BitInteger length) @ elements

    | Nil          -> [| Tag.NilExt |]

    | Binary b     -> [| Tag.BinaryExt |] @ (encode32BitInteger (Array.length b)) @ b

    | List l       -> let length = List.length l
                      let allSmallIntegers = isAllSmallIntegers l
                      let elements = List.toArray l
                                     |> Array.map oldEncode
                                     |> Array.concat
                      if allSmallIntegers && (is16Bit length) then
                          [| Tag.StringExt |] @ (encode16BitInteger length) @ elements
                      else
                          [| Tag.ListExt |] @ (encode32BitInteger length) @ elements @ [| Tag.NilExt |]

    | BigInteger i -> let sign = if BigInteger.IsPositive i then 0uy else 1uy
                      let bytes = BigInteger.Abs(i).ToByteArray(isUnsigned = true, isBigEndian = false)
                      let length = Array.length bytes
                      if isByte length then
                          [| Tag.SmallBigExt; byte length; sign |] @ bytes
                      else
                          [| Tag.LargeBigExt |] @ (encode32BitInteger length) @ [| sign |] @ bytes

    | Atom s       -> let atomName = Text.Encoding.UTF8.GetBytes(s)
                      let length = Array.length atomName
                      if isByte length then
                          [| Tag.SmallAtomUTF8Ext; byte length |] @ atomName
                      else
                          [| Tag.AtomUTF8Ext |] @ (encode16BitInteger length) @ atomName

    | Map pairs    -> let numberOfPairs = List.length pairs
                      let arity = encode32BitInteger numberOfPairs
                      let pairs = List.toArray pairs
                                  |> Array.map (fun (key, value) -> Array.concat [|oldEncode key; oldEncode value|])
                                  |> Array.concat
                      [|Tag.MapExt|] @ arity @ pairs
    |> Array.append [| Tag.Version |]

/// Encode an Erlang term value to bytes and write them to the given BinaryWriter's
/// underlying Stream according to the Erlang external term format.
/// See https://www.erlang.org/doc/apps/erts/erl_ext_dist.html
let encodeTerm (writer: BinaryWriter) (term: ErlangTerm) =
    let rec encodeTermHelper (term: ErlangTerm) =
        match term with
        | Integer i    -> if isByte i then
                              writer.Write [| Tag.SmallIntegerExt; byte i |]
                          else
                              writer.Write Tag.IntegerExt
                              writer.Write (encode32BitInteger i)

        | Float f      -> writer.Write Tag.NewFloatExt
                          writer.Write (encodeFloat f)

        | Tuple t      -> let length = List.length t
                          if isByte length then
                              writer.Write [| Tag.SmallTupleExt; byte length |]
                              List.iter encodeTermHelper t
                          else
                              writer.Write Tag.LargeTupleExt
                              writer.Write (encode32BitInteger length)
                              List.iter encodeTermHelper t

        | Nil          -> writer.Write Tag.NilExt

        | Binary b     -> writer.Write Tag.BinaryExt
                          writer.Write (encode32BitInteger (Array.length b))
                          writer.Write b

        | List l       -> let length = List.length l
                          let allSmallIntegers = isAllSmallIntegers l
                          if allSmallIntegers && (is16Bit length) then
                              writer.Write Tag.StringExt
                              writer.Write (encode16BitInteger length)
                              List.iter encodeTermHelper l
                          else
                              writer.Write Tag.ListExt
                              writer.Write (encode32BitInteger length)
                              List.iter encodeTermHelper l
                              writer.Write Tag.NilExt

        | BigInteger i -> let sign = if BigInteger.IsPositive i then 0uy else 1uy
                          let bytes = BigInteger.Abs(i).ToByteArray(isUnsigned = true, isBigEndian = false)
                          let length = Array.length bytes
                          if isByte length then
                              writer.Write [| Tag.SmallBigExt; byte length; sign |]
                              writer.Write bytes
                          else
                              writer.Write Tag.LargeBigExt
                              writer.Write (encode32BitInteger length)
                              writer.Write sign
                              writer.Write bytes

        | Atom s       -> let atomName = Text.Encoding.UTF8.GetBytes(s)
                          let length = Array.length atomName
                          if isByte length then
                              writer.Write [| Tag.SmallAtomUTF8Ext; byte length |]
                              writer.Write atomName
                          else
                              writer.Write Tag.AtomUTF8Ext
                              writer.Write (encode16BitInteger length)
                              writer.Write atomName

        | Map pairs    -> let numberOfPairs = List.length pairs
                          let arity = encode32BitInteger numberOfPairs
                          writer.Write Tag.MapExt
                          writer.Write arity
                          pairs
                          |> List.iter (fun (key, value) -> encodeTermHelper key
                                                            encodeTermHelper value)
    writer.Write(Tag.Version)
    encodeTermHelper term

/// Encode an Erlang term value into an array of bytes according to the
/// Erlang external term format.
/// See https://www.erlang.org/doc/apps/erts/erl_ext_dist.html
let encodeTermToBytes (term: ErlangTerm) =
    use memoryStream = new MemoryStream()
    use binaryWriter = new BinaryWriter(memoryStream)
    encodeTerm binaryWriter term
    memoryStream.ToArray()
