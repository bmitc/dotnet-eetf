module Tests

open System
open Xunit
open FsUnit.Xunit

open ErlangExternalTermFormat

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

let parseTermToBinaryResult (s: string) =
    let stringSplitOptions = StringSplitOptions.TrimEntries &&& StringSplitOptions.RemoveEmptyEntries
    s.Replace("<", "")
     .Replace(">", "")
     .Split(',', stringSplitOptions)
    |> Array.map (fun s -> s.Trim())
    |> Array.map byte

let writeAndDecode (s: string) =
    s
    |> writeAndRead
    |> parseTermToBinaryResult
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
//        match writeAndRead "atom" |> (fun (s: string) -> s.Trim()) |> parseTermToBinaryResult with
//        | AtomExt s -> $":atom"
//        | _ -> "fail"
//    result |> should equal ":atom"
//    //writeAndRead ":atom" |> parseTermToBinaryResult |> should equal [|131; 100; 0; 4; 97; 116; 111; 109|]

//[<Fact>]
//let ``Decode as atom`` () =
//    "testing" |> writeAndRead |> parseTermToBinaryResult |> decodeAsAtom |> should equal (Some "testing")

open Hedgehog.Xunit

(*[<Property>]
let ``Decoding atoms`` (atom: string) =
    atom
    |> writeAndRead
    |> parseTermToBinaryResult
    |> decodeAsAtom
    |> should equal (Some atom)*)


[<Property>]
let ``Reversing a list twice yields the original list`` (xs: int list) =
    List.rev (List.rev xs) = xs

[<Fact>]
let ``Decode atom from stream`` () =
    ":testing" |> writeAndDecode |> should equal (Erlang.Atom "testing")

[<Fact>]
let ``Decode tuple from stream`` () =
    @"{:ok, :another}" |> writeAndDecode |> should equal (Erlang.Tuple [Erlang.Atom "ok"; Erlang.Atom "another"])

[<Fact>]
let ``Decode tuple integers and floats from stream`` () =
    @"{1, 255, 10000, 3.14}" |> writeAndDecode
    |> should equal (Erlang.Tuple [Erlang.SmallInteger 1uy; Erlang.SmallInteger 255uy; Erlang.Integer 10_000; Erlang.Float 3.14])

[<Fact>]
let ``Decode list from stream`` () =
    @"[1, 255, 10000, 3.14]" |> writeAndDecode
    |> should equal (Erlang.List [Erlang.SmallInteger 1uy; Erlang.SmallInteger 255uy; Erlang.Integer 10_000; Erlang.Float 3.14; Erlang.Nil])

[<Fact>]
let ``Decode a small list of bytes (small integers) from stream`` () =
    @"[0, 1, 2, 3, 4]" |> writeAndDecode
    |> should equal (Erlang.List [Erlang.SmallInteger 0uy; Erlang.SmallInteger 1uy; Erlang.SmallInteger 2uy; Erlang.SmallInteger 3uy; Erlang.SmallInteger 4uy])

[<Fact>]
let ``Decode a big integer from stream`` () =
    let bigInteger = 100000000000000000000000000000000000000000000000000000I
    string bigInteger |> writeAndDecode |> should equal (Erlang.BigInteger bigInteger)
