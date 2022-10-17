module Tests

open System
open Xunit
open FsUnit.Xunit

open EETF.Decode

//let writeAndRead (s: string) =
//    use p = new System.Diagnostics.Process()
//    p.StartInfo.WorkingDirectory <- Environment.CurrentDirectory
//    p.StartInfo.FileName <- "powershell"
//    p.StartInfo.Arguments <- $"elixir term_to_binary.exs {s}"
//    p.StartInfo.UseShellExecute <- false
//    p.StartInfo.RedirectStandardOutput <- true
//    p.Start() |> ignore
//    let output = p.StandardOutput.ReadToEnd()
//    p.WaitForExit()
//    output.Trim()

let writeAndRead (s: string) =
    use p = new System.Diagnostics.Process()
    p.StartInfo.WorkingDirectory <- System.IO.Path.Join(Environment.CurrentDirectory, @"../../../../../../elixir/string_parser")
    p.StartInfo.FileName <- "powershell"
    p.StartInfo.Arguments <- $"elixir scripts/string_parser.exs '{s}'"
    p.StartInfo.UseShellExecute <- false
    p.StartInfo.RedirectStandardOutput <- true
    p.Start() |> ignore
    let output = p.StandardOutput.ReadToEnd()
    p.WaitForExit()
    output.Trim()

let writeAndDecode (s: string) =
    s
    |> writeAndRead
    |> convertTermStringToBytes
    |> decodeTermFromBytes

//[<Fact>]
//let ``Simple atom test`` () =
//    let result =
//        match [|131; 100; 0; 4; 97; 116; 111; 109|] |> Array.map byte with
//        | AtomExt s -> $":{s}"
//        | _ -> "fail"
//    result |> should equal ":atom"

//[<Fact>]
//let ``Simple term to binary test`` () =
//    let result =
//        match writeAndRead "atom" |> (fun (s: string) -> s.Trim()) |> convertTermStringToBytes with
//        | AtomExt s -> $":atom"
//        | _ -> "fail"
//    result |> should equal ":atom"
//    //writeAndRead ":atom" |> convertTermStringToBytes |> should equal [|131; 100; 0; 4; 97; 116; 111; 109|]

//[<Fact>]
//let ``Decode as atom`` () =
//    "testing" |> writeAndRead |> convertTermStringToBytes |> decodeAsAtom |> should equal (Some "testing")

open Hedgehog.Xunit

(*[<Property>]
let ``Decoding atoms`` (atom: string) =
    atom
    |> writeAndRead
    |> convertTermStringToBytes
    |> decodeAsAtom
    |> should equal (Some atom)*)


[<Property>]
let ``Reversing a list twice yields the original list`` (xs: int list) =
    List.rev (List.rev xs) = xs

[<Fact>]
let ``Converting a binary term string to an array of bytes`` () =
    // The binary string "<<131, 97, 1>>" represents the Elixir integer `1`.
    // Several string formats are supported, although "<<131, 97, 1>>" is the most common in Elixir and Erlang.
    @"<<131, 97, 1>>" |> convertTermStringToBytes |> should equal [|131uy; 97uy; 1uy|]
    @"<<131 97 1>>" |> convertTermStringToBytes |> should equal [|131uy; 97uy; 1uy|]
    @"131, 97, 1" |> convertTermStringToBytes |> should equal [|131uy; 97uy; 1uy|]
    @"131 97 1" |> convertTermStringToBytes |> should equal [|131uy; 97uy; 1uy|]

[<Fact>]
let ``Decode atom from stream`` () =
    ":testing" |> writeAndDecode |> should equal (Erlang.Atom "testing")

[<Fact>]
let ``Decode tuple from stream`` () =
    @"{:ok, :another}" |> writeAndDecode |> should equal (Erlang.Tuple [Erlang.Atom "ok"; Erlang.Atom "another"])

[<Fact>]
let ``Decode tuple integers and floats from stream`` () =
    @"{1, 255, 10000, 3.14}" |> writeAndDecode
    |> should equal (Erlang.Tuple [Erlang.Integer 1; Erlang.Integer 255; Erlang.Integer 10_000; Erlang.Float 3.14])

[<Fact>]
let ``Decode list from stream`` () =
    @"[1, 255, 10000, 3.14]" |> writeAndDecode
    |> should equal (Erlang.List [Erlang.Integer 1; Erlang.Integer 255; Erlang.Integer 10_000; Erlang.Float 3.14; Erlang.Nil])

[<Fact>]
let ``Decode a small list of bytes (small integers) from stream`` () =
    @"[0, 1, 2, 3, 4]" |> writeAndDecode
    |> should equal (Erlang.List [Erlang.Integer 0; Erlang.Integer 1; Erlang.Integer 2; Erlang.Integer 3; Erlang.Integer 4])

[<Fact>]
let ``Decode a big integer from stream`` () =
    let bigInteger = 100000000000000000000000000000000000000000000000000000I
    string bigInteger |> writeAndDecode |> should equal (Erlang.BigInteger bigInteger)

[<Fact>]
let ``Decode a map from stream`` () =
    @"%{a: 1, b: 2}" |> writeAndDecode |> should equal (Erlang.Map [(Erlang.Atom "a", Erlang.Integer 1); (Erlang.Atom "b", Erlang.Integer 2)])
