module List.More exposing (getSomethingDifferent)

import Random


getSomethingDifferent : a -> List a -> Random.Generator a
getSomethingDifferent theItem theList =
    let
        filtered =
            List.filter (\a -> theItem /= a) theList

        getItemAt index =
            let
                somethingDifferentResult =
                    filtered
                        |> List.drop (index - 1)
                        |> List.head
            in
            case somethingDifferentResult of
                Just somethingDifferent ->
                    somethingDifferent

                Nothing ->
                    theItem

        randomItemResult =
            filtered
                |> List.length
                |> Random.int 0
                |> Random.map getItemAt
    in
    randomItemResult
