/// Provides a type representing Erlang terms
module EETF.Type

open System.Text

/// Represents Erlang terms that can be encoded from F# to the Erlang external term
/// format or decoded from the term format to F#
type ErlangTerm =
    | Integer of int
    | Float of float
    | Tuple of ErlangTerm list
    | Nil
    | Binary of byte[]
    | List of ErlangTerm list
    | BigInteger of bigint
    | Atom of string
    | Map of (ErlangTerm * ErlangTerm) list

let binaryFromString (string: string) =
    string
    |> Encoding.Unicode.GetBytes
    |> Binary
