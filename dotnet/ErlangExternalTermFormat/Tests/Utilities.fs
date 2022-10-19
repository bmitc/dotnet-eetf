module EETF.Tests.Utilities

open System
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

let writeAndReadAsBytes (s: string) =
    s
    |> writeAndRead
    |> convertTermStringToBytes

let writeAndDecode (s: string) =
    s
    |> writeAndRead
    |> convertTermStringToBytes
    |> decodeTermFromBytes
