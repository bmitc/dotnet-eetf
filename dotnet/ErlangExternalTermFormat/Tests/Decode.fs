module EETF.Tests.Decode

open System
open Xunit
open FsUnit.Xunit

open EETF.Type
open EETF.Decode

open EETF.Tests.Utilities

[<Fact>]
let ``Converting a binary term string to an array of bytes`` () =
    // The binary string "<<131, 97, 1>>" represents the Elixir integer `1`.
    // Several string formats are supported, although "<<131, 97, 1>>" is the most common in Elixir and Erlang.
    @"<<131, 97, 1>>" |> convertTermStringToBytes |> should equal [|131uy; 97uy; 1uy|]
    @"<<131,97,1>>" |> convertTermStringToBytes |> should equal [|131uy; 97uy; 1uy|]
    @"<<131 97 1>>" |> convertTermStringToBytes |> should equal [|131uy; 97uy; 1uy|]
    @"131, 97, 1" |> convertTermStringToBytes |> should equal [|131uy; 97uy; 1uy|]
    @"131,97,1" |> convertTermStringToBytes |> should equal [|131uy; 97uy; 1uy|]
    @"131 97 1" |> convertTermStringToBytes |> should equal [|131uy; 97uy; 1uy|]

[<Fact>]
let ``Decode atom from stream`` () =
    ":testing" |> writeAndDecode |> should equal (ErlangTerm.Atom "testing")
    //":\"atom with spaces\"" |> writeAndDecode |> should equal (Erlang.Atom "\"atom with spaces\"")

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
let ``Decode an improper list from stream`` () =
    @"[1, 255, 10000 | 3.14]" |> writeAndDecode
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
