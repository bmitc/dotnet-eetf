module Microsoft.FSharp.Collections.List

/// Removes the last element of a list
let removeLast (lst: 'T list) =
    match lst with
    | [] -> []
    | _  -> lst
            |> List.rev
            |> List.tail
            |> List.rev

/// Replaces the first element of a list using the given replacer function
let replaceFirstBy (replacer: 'T -> 'T) (lst: 'T list) =
    (replacer lst.Head) :: lst.Tail

/// Replaces the last element of a list using the given replacer function.
/// The replacer returns a list because it may be desirable to replace the
/// last element with a list rather than a single element. In the case of
/// replacing with a single element, return a single element list.
let replaceLastBy (replacer: 'T -> 'T list) (lst: 'T list) =
    let rec map lst =
        match lst with
            | []      -> []
            | x :: [] -> (replacer x)
            | x :: xs -> x :: map xs
    map lst
