/// Functions for decoding Erlang terms encoded in the external term format
module EETF.Decode

open System
open System.Buffers.Binary
open System.IO
open System.Numerics

open Microsoft.FSharp.Collections

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

/// Converts a string representing an Erlang binary to a byte array. Supported format examples
/// are "<<131, 97, 1>>"`, "<<131 97 1>>", "131, 97, 1", "131 97 1". Extra whitespace is ignored.
/// Any other character besides number, comma, '<', '>', or whitespace will cause an error.
let convertTermStringToBytes (s: string) =
    try
        let stringSplitOptions = StringSplitOptions.TrimEntries + StringSplitOptions.RemoveEmptyEntries
        s.Trim('<').Trim('>').Replace(',', ' ').Split(' ', stringSplitOptions)
        |> Array.map byte
    with
        | :? System.FormatException ->
            let message = "String parsing or byte conversion failed. This is likely due to an improperly formatted binary string."
            raise (System.ArgumentException message)

let private handleErlangList (lst: ErlangTerm list) =
    match List.tryLast lst with
    | Some Nil -> List.removeLast lst
    | _        -> lst

/// Decode an Erlang term from the BinaryReader's underlying Stream that contains
/// an Erlang term encoded according to the Erlang external term format.
/// See https://www.erlang.org/doc/apps/erts/erl_ext_dist.html
let rec decodeTerm (reader: BinaryReader) =
    match reader.ReadByte() with
    | Tag.SmallIntegerExt  -> reader.ReadByte()
                              |> int
                              |> ErlangTerm.Integer

    | Tag.IntegerExt       -> reader.ReadBytes(4)
                              |> BinaryPrimitives.ReadInt32BigEndian
                              |> ErlangTerm.Integer

    | Tag.FloatExt         -> reader.ReadBytes(31)
                              |> Text.Encoding.ASCII.GetString
                              |> float
                              |> ErlangTerm.Float

    | Tag.SmallTupleExt    -> let arity = reader.ReadByte() |> int
                              ErlangTerm.Tuple [for _ in 1..arity -> decodeTerm reader]

    | Tag.LargeTupleExt    -> let arity = reader.ReadBytes(2) |> BinaryPrimitives.ReadUInt16BigEndian |> int
                              ErlangTerm.Tuple [for _ in 1..arity -> decodeTerm reader]

    | Tag.MapExt           -> let arity = reader.ReadBytes(4) |> BinaryPrimitives.ReadUInt32BigEndian |> int
                              [for _ in 1..arity -> (decodeTerm reader, decodeTerm reader)]
                              |> ErlangTerm.Map

    | Tag.NilExt           -> ErlangTerm.Nil

    | Tag.StringExt        -> reader.ReadBytes(2) // read length from two bytes
                              |> BinaryPrimitives.ReadUInt16BigEndian
                              |> int
                              |> reader.ReadBytes
                              |> Array.map (int >> ErlangTerm.Integer)
                              |> Array.toList
                              |> ErlangTerm.List

    | Tag.ListExt          -> let length = reader.ReadBytes(4) // read length from four bytes
                                           |> BinaryPrimitives.ReadUInt32BigEndian
                                           |> int
                              [for _ in 1..(length + 1) -> decodeTerm reader]
                              |> handleErlangList
                              |> ErlangTerm.List

    | Tag.BinaryExt        -> reader.ReadBytes(4) // read length from four bytes
                              |> BinaryPrimitives.ReadUInt32BigEndian
                              |> int
                              |> reader.ReadBytes
                              |> ErlangTerm.Binary

    | Tag.SmallBigExt      -> let length = reader.ReadByte() |> int
                              let sign =
                                  match reader.ReadByte() with
                                  | 0uy     -> 1
                                  | 1uy | _ -> -1
                              let B = 256I
                              reader.ReadBytes(length)
                              |> Array.mapi (fun index byte -> (bigint byte) * (pown B index))
                              |> Array.sum
                              |> (fun i -> BigInteger.Multiply(i, bigint sign))
                              |> ErlangTerm.BigInteger

    | Tag.LargeBigExt      -> let length = reader.ReadBytes(4) |> BinaryPrimitives.ReadUInt32BigEndian |> int
                              let sign =
                                  match reader.ReadByte() with
                                  | 0uy     -> 1
                                  | 1uy | _ -> -1
                              let B = 256I
                              reader.ReadBytes(length)
                              |> Array.mapi (fun index byte -> (bigint byte) * (pown B index))
                              |> Array.sum
                              |> (fun i -> BigInteger.Multiply(i, bigint sign))
                              |> ErlangTerm.BigInteger

    | Tag.NewFloatExt      -> reader.ReadBytes(8)
                              |> BinaryPrimitives.ReadDoubleBigEndian
                              |> ErlangTerm.Float

    | Tag.AtomUTF8Ext      -> reader.ReadBytes(2) // read length from two bytes
                              |> BinaryPrimitives.ReadUInt16BigEndian
                              |> int
                              |> reader.ReadBytes
                              |> Text.Encoding.UTF8.GetString
                              |> ErlangTerm.Atom

    | Tag.SmallAtomUTF8Ext -> reader.ReadByte() // read length from one byte
                              |> int
                              |> reader.ReadBytes
                              |> Text.Encoding.UTF8.GetString
                              |> ErlangTerm.Atom

    | Tag.AtomExt          -> reader.ReadBytes(2) // read length from two bytes
                              |> BinaryPrimitives.ReadUInt16BigEndian
                              |> int
                              |> reader.ReadBytes
                              |> Text.Encoding.Latin1.GetString
                              |> ErlangTerm.Atom

    | Tag.SmallAtomExt     -> reader.ReadByte() // read length from one byte
                              |> int
                              |> reader.ReadBytes
                              |> Text.Encoding.Latin1.GetString
                              |> ErlangTerm.Atom
    | _                    -> raise (System.ArgumentException "The byte stream does not represent a supported Erlang term.")

/// Decode an Erlang term from a byte array that contains an Erlang
/// term encoded according to the Erlang external term format.
/// See https://www.erlang.org/doc/apps/erts/erl_ext_dist.html
let decodeTermFromBytes (bytes: byte[]) =
    match Array.splitAt 1 bytes with
    | ( [|Tag.Version|], elements ) -> use memoryStream = new MemoryStream(elements)
                                       use binaryReader = new BinaryReader(memoryStream)
                                       decodeTerm binaryReader
    | _                             -> raise (System.Exception "Failed to decode Erlang binary")

/// Decode an Erlang term when represented as a string of bytes. Supported format examples
/// are "<<131, 97, 1>>"`, "<<131 97 1>>", "131, 97, 1", "131 97 1". Extra whitespace is ignored.
/// Any other character besides number, comma, '<', '>', or whitespace will cause an error.
/// See https://www.erlang.org/doc/apps/erts/erl_ext_dist.html
let decodeTermFromString = convertTermStringToBytes >> decodeTermFromBytes
