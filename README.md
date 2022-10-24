# EETF.NET
 
This is an F# library to encode and decode values in the Erlang external term format.
 
The [Erlang external term format](https://www.erlang.org/doc/apps/erts/erl_ext_dist.html) defines binary representations for various Erlang terms. This library allows F# to decode a subset of these terms to an F# `ErlangTerm` datatype, which is a normal F# discriminated union. Then this library allows encoding arbitrary values expressed with the `ErlangTerm` type back down to binary in valid Erlang external term format.

In Erlang, terms and binary are coverted back and forth to each other using `term_to_binary/1` and `binary_to_term/1`. In Elixir, you can use `:erlang.term_to_binary/1` and `:erlang.binary_to_term/1`. The terms are encoded to the same binary format.

**Note**: This is a work in progress! At present, the encoding and decoding works well enough, but more tests need to be added to ensure the best coverage possible. This includes adding tests to encode data using F# and then decode it in Elixir with `:erlang.binary_to_term/1` to see if we get what's expected.

## Examples

Better examples are to come, but for now, the tests given some indication of how this works. In the test, the given string is handed off to an Elixir string which parses it into an Elixir term and then converts it binary form using `:erlang.term_to_binary/1`. From there, the bytes are writting in string form to the command line, which is then read in by F# and then decoded.

```fsharp
[<Fact>]
let ``Decode tuple from stream`` () =
    @"{:ok, :another}" |> writeAndDecode |> should equal (ErlangTerm.Tuple [ErlangTerm.Atom "ok"; ErlangTerm.Atom "another"])

[<Fact>]
let ``Decode tuple integers and floats from stream`` () =
    @"{1, 255, 10000, 3.14}" |> writeAndDecode
    |> should equal (ErlangTerm.Tuple [ErlangTerm.Integer 1; ErlangTerm.Integer 255; ErlangTerm.Integer 10_000; ErlangTerm.Float 3.14])

[<Fact>]
let ``Decode list from stream`` () =
    @"[1, 255, 10000, 3.14]" |> writeAndDecode
    |> should equal (ErlangTerm.List [ErlangTerm.Integer 1; ErlangTerm.Integer 255; ErlangTerm.Integer 10_000; ErlangTerm.Float 3.14])

[<Fact>]
let ``Decode a small list of bytes (small integers) from stream`` () =
    @"[0, 1, 2, 3, 4]" |> writeAndDecode
    |> should equal (ErlangTerm.List [ErlangTerm.Integer 0; ErlangTerm.Integer 1; ErlangTerm.Integer 2; ErlangTerm.Integer 3; ErlangTerm.Integer 4])

[<Fact>]
let ``Decode a big integer from stream`` () =
    let bigInteger = 100000000000000000000000000000000000000000000000000000I
    string bigInteger |> writeAndDecode |> should equal (ErlangTerm.BigInteger bigInteger)

[<Fact>]
let ``Decode a map from stream`` () =
    @"%{a: 1, b: 2}" |> writeAndDecode |> should equal (ErlangTerm.Map [(ErlangTerm.Atom "a", ErlangTerm.Integer 1); (ErlangTerm.Atom "b", ErlangTerm.Integer 2)])
```

## Supported Tags

The following terms/tags are supported:
* SMALL_INTEGER_EXT
* INTEGER_EXT
* FLOAT_EXT
* SMALL_TUPLE_EXT
* LARGE_TUPLE_EXT
* MAP_EXT
* NIL_EXT
* STRING_EXT
* LIST_EXT
* BINARY_EXT
* SMALL_BIG_EXT
* LARGE_BIG_EXT
* NEW_FLOAT_EXT
* ATOM_UTF8_EXT
* SMALL_ATOM_UTF8_EXT
* ATOM_EXT
* SMALL_ATOM_EXT
