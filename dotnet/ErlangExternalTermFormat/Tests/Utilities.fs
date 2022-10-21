﻿module EETF.Tests.Utilities

open System
open System.Runtime.InteropServices

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

/// Write the string representing an Elixir term to an Elixir script that converts the
/// string to an Elixir term and then a binary string.
/// Example: writeAndRead ":test" = "<<131, 100, 0, 4, 116, 101, 115, 116>>"
let writeAndRead (s: string) =
    let filename =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
            "powershell"
        else
            "bash"
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

/// Write the string representing an Elixir term to an Elixir script that converts the
/// string to a byte array representing the Elixir term.
/// Example: writeAndReadAsBytes ":test" = [|131; 100; 0; 4; 116; 101; 115; 116|]
let writeAndReadAsBytes (s: string) =
    s
    |> writeAndRead
    |> convertTermStringToBytes

/// Write the string representing an Elixir term to an Elixir script that converts the
/// string to a byte array representing the Elixir term and then decode the byte array
/// to an Erlang term.
/// Example: writeAndDecode ":test" = ErlangTerm.Atom "test"
let writeAndDecode (s: string) =
    s
    |> writeAndRead
    |> convertTermStringToBytes
    |> decodeTermFromBytes
