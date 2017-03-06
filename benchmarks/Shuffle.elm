module Shuffle exposing (shuffle)

import Random.Pcg as Random exposing (Generator)


shuffle : List a -> Generator (List a)
shuffle aList =
    let
        shuffleHelper : ( List a, List a ) -> Generator (List a)
        shuffleHelper lists =
            helper lists
                |> Random.andThen
                    (\( left, right ) ->
                        case right of
                            [] ->
                                Random.constant left

                            _ ->
                                shuffleHelper ( left, right )
                    )
    in
        shuffleHelper ( [], aList )


helper : ( List a, List a ) -> Generator ( List a, List a )
helper ( left, right ) =
    Random.int 1 (List.length right)
        |> Random.map
            (\idx ->
                let
                    before =
                        List.take (idx - 1) right

                    self =
                        List.drop (idx - 1) right |> List.take 1

                    after =
                        List.drop idx right
                in
                    ( left ++ self, before ++ after )
            )
