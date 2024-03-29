﻿module EETF.Tests.Properties

open Hedgehog
open Hedgehog.Xunit

open EETF.Type
open EETF.Encode
open EETF.Decode

[<Literal>]
let numberOfTests = 100_000<tests>

type ListGenConfig =
  static member __ =
    GenX.defaults
    |> AutoGenConfig.addGenerator
        (Gen.list (Range.linear 0 100)
                  (Gen.int32 (Range.linearBounded ())))

[<Property(Tests = numberOfTests)>]
let ``Encoding and decoding integers`` (integer: int) =
    ErlangTerm.Integer integer |> encodeTermToBytes |> decodeTermFromBytes = ErlangTerm.Integer integer

[<Property(Tests = numberOfTests)>]
let ``Encoding and decoding floats`` (f: float) =
    ErlangTerm.Float f |> encodeTermToBytes |> decodeTermFromBytes = ErlangTerm.Float f

[<Property(Tests = numberOfTests)>]
let ``Encoding and decoding byte arrays`` (bytes: byte[]) =
    ErlangTerm.Binary bytes |> encodeTermToBytes |> decodeTermFromBytes = ErlangTerm.Binary bytes

[<Property(Tests = numberOfTests, AutoGenConfig = typeof<ListGenConfig>)>]
let ``Encoding and decoding lists of integers`` (list: int list) =
    let listOfIntegers =
        list
        |> List.map ErlangTerm.Integer
        |> ErlangTerm.List
    listOfIntegers |> encodeTermToBytes |> decodeTermFromBytes = listOfIntegers

[<Property(Tests = numberOfTests)>]
let ``Encoding and decoding big integers`` (bigInteger: bigint) =
    ErlangTerm.BigInteger bigInteger |> encodeTermToBytes |> decodeTermFromBytes = ErlangTerm.BigInteger bigInteger

[<Property(Tests = numberOfTests)>]
let ``Encoding and decoding atoms`` (atomName: string) =
    ErlangTerm.Atom atomName |> encodeTermToBytes |> decodeTermFromBytes = ErlangTerm.Atom atomName
