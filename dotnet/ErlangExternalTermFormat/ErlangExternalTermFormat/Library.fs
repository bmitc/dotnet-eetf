module ErlangExternalTermFormat

open System
open System.Buffers.Binary
open System.IO

open EETF

[<RequireQualifiedAccess>]
type Erlang =
    | SmallInteger of uint8
    | Integer of int
    | Float of float
    | Tuple of Erlang list
    | Nil
    | Binary of byte[]
    | List of Erlang list
    | BigInteger of bigint
    | SmallBig of bigint
    | LargeBig of bigint
    | Atom of string
    | AtomUTF8 of string
    | SmallAtomUTF8 of string
    | AtomExt of string
    | SmallAtomExt of string

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

//let decodeTerm (bytes: byte[]) =
//    let keepDecoding ((term, bytes): Erlang * byte[]) =
//        match bytes with
//        | [||] -> term
//        | 
//    match bytes with
//    | AtomUTF8Ext (atom, bytes)      -> atom
//    | SmallAtomUTF8Ext (atom, bytes) -> atom
//    | AtomExt (atom, bytes)          -> atom
//    | SmallAtomExt (atom, bytes)     -> atom
//    | _                              -> raise (System.Exception "Failed to decode Erlang binary")


let rec decodeTermFromStream (binary: BinaryReader) =
    match binary.ReadByte() with
    | Tag.SmallIntegerExt  -> binary.ReadByte()
                              |> Erlang.SmallInteger

    | Tag.IntegerExt       -> binary.ReadBytes(4)
                              |> BinaryPrimitives.ReadInt32BigEndian
                              |> Erlang.Integer

    | Tag.FloatExt         -> binary.ReadBytes(31)
                              |> Text.Encoding.ASCII.GetString
                              |> float
                              |> Erlang.Float

    | Tag.SmallTupleExt    -> let arity = binary.ReadByte() |> int
                              Erlang.Tuple [for x in 0..(arity-1) -> decodeTermFromStream binary]

    | Tag.LargeTupleExt    -> let arity = binary.ReadBytes(2) |> BinaryPrimitives.ReadUInt16BigEndian |> int
                              Erlang.Tuple [for x in 0..(arity-1) -> decodeTermFromStream binary]

    | Tag.NilExt           -> Erlang.Nil

    | Tag.StringExt        -> binary.ReadBytes(2) // read length from two bytes
                              |> BinaryPrimitives.ReadUInt16BigEndian
                              |> int
                              |> binary.ReadBytes
                              |> Array.map Erlang.SmallInteger
                              |> Array.toList
                              |> Erlang.List

    | Tag.ListExt          -> let length = binary.ReadBytes(4) // read length from four bytes
                                           |> BinaryPrimitives.ReadUInt32BigEndian
                                           |> int
                              Erlang.List [for x in 0..length -> decodeTermFromStream binary]

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

let decodeTermFromBytes (bytes: byte[]) =
    match Array.splitAt 1 bytes with
    | ( [|131uy|], elements ) -> elements |> MemoryStream |> BinaryReader |> decodeTermFromStream
    | _ -> raise (System.Exception "Failed to decode Erlang binary")