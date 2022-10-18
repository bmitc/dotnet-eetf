module EETF.Type

/// Represents Erlang types that can be encoded from F# to the Erlang external term
/// format or decoded from the term format to F#
[<RequireQualifiedAccess>]
type Erlang =
    | Integer of int
    | Float of float
    | Tuple of Erlang list
    | Nil
    | Binary of byte[]
    | List of Erlang list
    | BigInteger of bigint
    | Atom of string
    | Map of (Erlang * Erlang) list
