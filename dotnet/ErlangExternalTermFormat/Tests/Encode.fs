module EETF.Tests.Encode

open System
open Xunit
open FsUnit.Xunit

open EETF.Type
open EETF.Encode
open EETF.Decode

open EETF.Tests.Utilities

[<Fact>]
let ``Encode atoms`` () =
    let expected = writeAndReadAsBytes @":testing"
    //Erlang.Atom "testing" |> encode |> should equal expected
    let testAtoms = ["testing"; "\"atom with spaces\""; "atom_with_1234_and_@_symbol"]
    for atom in testAtoms do
        Erlang.Atom atom
        |> encode
        |> decodeTermFromBytes
        |> should equal (Erlang.Atom atom)

[<Fact>]
let ``Encode small integers`` () =
    let testIntegers = [0; 1; 99; 101; 254; 255]
    for integer in testIntegers do
        Erlang.Integer integer
        |> encode
        |> decodeTermFromBytes
        |> should equal (Erlang.Integer integer)

[<Fact>]
let ``Encode integers`` () =
    let testIntegers = [0; 1; -2; 101; -1_000; 34424; -6559825; -2_147_483_648; 2_147_483_647]
    for integer in testIntegers do
        Erlang.Integer integer
        |> encode
        |> decodeTermFromBytes
        |> should equal (Erlang.Integer integer)

[<Fact>]
let ``Encode small big integers`` () =
    let testBigIntegers =
        [0I; 1I; -2I; -2_147_483_648I; 2_147_483_647I;
         10000000000000000000000000000000000000000I;
         -1000000000000000000000000000987239875923475829378I]
    for bigInteger in testBigIntegers do
        Erlang.BigInteger bigInteger
        |> encode
        |> decodeTermFromBytes
        |> should equal (Erlang.BigInteger bigInteger)

[<Fact>]
let ``Encode big big integers`` () =
    let testBigIntegers =
        [1234587088709871092387509127346298734238721348721349872134908721349876324876324875621340987213498721340987213498721348763487621348709871298721340987213487963245876345987123498723498762349857623587612340987123419873245987632458761234980721349872345987632587612340987123409872134098723458762134263873219874897129873409172430912785632846873263546325410279843098719207384172094387324563265712304987123409871243098732487612430987213409872134098725624319872340657802134987256123478097234987324561234087348761234987324782134I;
         -10987123409871234098721340987213490873245786328761234098712430987124398723409872345873245678324567893246587213409871243098712349872345786324587612340987123498723458762345876123498712340987123465329871234987340986123409872134987623450987123481234098712340987123498723458796234587632487612340987123409871234098721349872134098723459876324598762345876124389712309872134098712348756987346578679862195367896324769821653083417813240987213409872134789632487632865098712349872134987213498721347632458763248712347632409872134I]
    for bigInteger in testBigIntegers do
        Erlang.BigInteger bigInteger
        |> encode
        |> decodeTermFromBytes
        |> should equal (Erlang.BigInteger bigInteger)
