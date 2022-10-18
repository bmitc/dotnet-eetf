/// Functions for decoding Erlang terms encoded in the external term format
module EETF.Decode

open System
open System.Buffers.Binary
open System.IO

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

let (|AtomUTF8Ext|_|) (bytes: byte[]) =
    match Seq.toList bytes with
    | Tag.AtomUTF8Ext :: l1 :: l2 :: rest ->
        let length = BinaryPrimitives.ReadUInt16BigEndian(ReadOnlySpan [|l1; l2|]) |> int
        let (atomBytes, remainingBytes) = List.splitAt length  rest
        let atom = atomBytes
                   |> Seq.toArray
                   |> Text.Encoding.UTF8.GetString
        Some (atom, remainingBytes |> List.toArray)
    | _ -> None

let (|SmallAtomUTF8Ext|_|) (bytes: byte[]) =
    match Seq.toList bytes with
    | Tag.SmallAtomUTF8Ext :: length :: rest ->
        let length = length |> int
        let (atomBytes, remainingBytes) = List.splitAt length  rest
        let atom = atomBytes
                   |> Seq.toArray
                   |> Text.Encoding.UTF8.GetString
        Some (atom, remainingBytes |> List.toArray)
    | _ -> None

let (|AtomExt|_|) (bytes: byte[]) =
    match Seq.toList bytes with
    | Tag.AtomExt :: l1 :: l2 :: rest ->
        let length = BinaryPrimitives.ReadUInt16BigEndian(ReadOnlySpan [|l1; l2|]) |> int
        let (atomBytes, remainingBytes) = List.splitAt length  rest
        let atom = atomBytes
                   |> Seq.toArray
                   |> Text.Encoding.Latin1.GetString
        Some (atom, remainingBytes |> List.toArray)
    | _ -> None

let (|SmallAtomExt|_|) (bytes: byte[]) =
    match Seq.toList bytes with
    | Tag.AtomExt :: length :: rest ->
        let length = length |> int
        let (atomBytes, remainingBytes) = List.splitAt length  rest
        let atom = atomBytes
                   |> Seq.toArray
                   |> Text.Encoding.Latin1.GetString
        Some (atom, remainingBytes |> List.toArray)
    | _ -> None

let (|Atom|_|) (bytes: byte[]) =
    match bytes with
    | AtomUTF8Ext atom      -> Some atom
    | SmallAtomUTF8Ext atom -> Some atom
    | AtomExt atom          -> Some atom
    | SmallAtomExt atom     -> Some atom
    | _                     -> None

let (|SmallTupleExt|_|) (bytes: byte[]) =
    match Seq.toList bytes with
    | Tag.SmallTupleExt :: arity :: elements ->
        let arity = arity |> int
        match arity with
        | 0 -> Some (Erlang.Tuple [])

let decodeAsAtomExt bytes =
    match (|AtomExt|_|) bytes with
    | Some x -> Some x
    | None -> None

let private handleErlangList (lst: Erlang list) =
    match List.tryLast lst with
    | Some Erlang.Nil -> List.removeLast lst
    | _               -> lst

/// Decode an Erlang term when represented as a BinaryReader
let rec decodeTerm (binary: BinaryReader) =
    match binary.ReadByte() with
    | Tag.SmallIntegerExt  -> binary.ReadByte()
                              |> int
                              |> Erlang.Integer

    | Tag.IntegerExt       -> binary.ReadBytes(4)
                              |> BinaryPrimitives.ReadInt32BigEndian
                              |> Erlang.Integer

    | Tag.FloatExt         -> binary.ReadBytes(31)
                              |> Text.Encoding.ASCII.GetString
                              |> float
                              |> Erlang.Float

    | Tag.SmallTupleExt    -> let arity = binary.ReadByte() |> int
                              Erlang.Tuple [for _ in 1..arity -> decodeTerm binary]

    | Tag.LargeTupleExt    -> let arity = binary.ReadBytes(2) |> BinaryPrimitives.ReadUInt16BigEndian |> int
                              Erlang.Tuple [for _ in 1..arity -> decodeTerm binary]

    | Tag.MapExt           -> let arity = binary.ReadBytes(4) |> BinaryPrimitives.ReadUInt32BigEndian |> int
                              [for _ in 1..arity -> (decodeTerm binary, decodeTerm binary)]
                              |> Erlang.Map

    | Tag.NilExt           -> Erlang.Nil

    | Tag.StringExt        -> binary.ReadBytes(2) // read length from two bytes
                              |> BinaryPrimitives.ReadUInt16BigEndian
                              |> int
                              |> binary.ReadBytes
                              |> Array.map (int >> Erlang.Integer)
                              |> Array.toList
                              |> Erlang.List

    | Tag.ListExt          -> let length = binary.ReadBytes(4) // read length from four bytes
                                           |> BinaryPrimitives.ReadUInt32BigEndian
                                           |> int
                              [for _ in 1..(length + 1) -> decodeTerm binary]
                              |> handleErlangList
                              |> Erlang.List

    | Tag.BinaryExt        -> binary.ReadBytes(4) // read length from four bytes
                              |> BinaryPrimitives.ReadUInt32BigEndian
                              |> int
                              |> binary.ReadBytes
                              |> Erlang.Binary

    | Tag.SmallBigExt      -> let length = binary.ReadByte() |> int
                              let sign =
                                  match binary.ReadByte() with
                                  | 0uy     -> 1
                                  | 1uy | _ -> -1
                              let B = 256I
                              binary.ReadBytes(length)
                              |> Array.mapi (fun index byte -> (bigint byte) * (pown B index))
                              |> Array.sum
                              |> Erlang.BigInteger

    | Tag.LargeBigExt      -> let length = binary.ReadBytes(4) |> BinaryPrimitives.ReadUInt32BigEndian |> int
                              let sign =
                                  match binary.ReadByte() with
                                  | 0uy     -> 1
                                  | 1uy | _ -> -1
                              let B = 256
                              binary.ReadBytes(length)
                              |> Array.mapi (fun index byte -> (int byte) * (pown B index))
                              |> Array.sum
                              |> bigint
                              |> Erlang.BigInteger

    | Tag.NewFloatExt      -> binary.ReadBytes(8)
                              |> BinaryPrimitives.ReadDoubleBigEndian
                              |> Erlang.Float

    | Tag.AtomUTF8Ext      -> binary.ReadBytes(2) // read length from two bytes
                              |> BinaryPrimitives.ReadUInt16BigEndian
                              |> int
                              |> binary.ReadBytes
                              |> Text.Encoding.UTF8.GetString
                              |> Erlang.Atom

    | Tag.SmallAtomUTF8Ext -> binary.ReadByte() // read length from one byte
                              |> int
                              |> binary.ReadBytes
                              |> Text.Encoding.UTF8.GetString
                              |> Erlang.Atom

    | Tag.AtomExt          -> binary.ReadBytes(2) // read length from two bytes
                              |> BinaryPrimitives.ReadUInt16BigEndian
                              |> int
                              |> binary.ReadBytes
                              |> Text.Encoding.Latin1.GetString
                              |> Erlang.Atom

    | Tag.SmallAtomExt     -> binary.ReadByte() // read length from one byte
                              |> int
                              |> binary.ReadBytes
                              |> Text.Encoding.Latin1.GetString
                              |> Erlang.Atom
    | _                    -> raise (System.ArgumentException "The byte stream does not represent a supported Erlang term.")

/// Decode an Erlang term when represented as a byte array
let decodeTermFromBytes (bytes: byte[]) =
    match Array.splitAt 1 bytes with
    | ( [|Tag.Version|], elements ) -> use memoryStream = new MemoryStream(elements)
                                       use binaryReader = new BinaryReader(memoryStream)
                                       decodeTerm binaryReader
    | _                             -> raise (System.Exception "Failed to decode Erlang binary")

/// Decode an Erlang term when represented as a string of bytes. Supported format examples
/// are "<<131, 97, 1>>"`, "<<131 97 1>>", "131, 97, 1", "131 97 1". Extra whitespace is ignored.
/// Any other character besides number, comma, '<', '>', or whitespace will cause an error.
let decodeTermFromString = convertTermStringToBytes >> decodeTermFromBytes
