module Serial.Modification exposing (suiteOfSize, main)

import Benchmark.Runner exposing (BenchmarkProgram, program)
import Benchmark exposing (..)
import Char
import Dict as Dict
import Dict.AVL as AvlDict
import Time


forValues : String -> Int -> (Int -> comparable) -> Benchmark
forValues name size keyer =
    let
        source =
            List.map
                (\key -> ( key, 1 ))
                keys

        keys =
            List.range 1 size
                |> List.map keyer
    in
        Benchmark.compare name
            (Benchmark.benchmark2 "Dict"
                (List.foldl (flip Dict.update (always (Just 2))))
                (Dict.fromList source)
                keys
            )
            (Benchmark.benchmark2 "Dict.AVL"
                (List.foldl (flip AvlDict.update (always (Just 2))))
                (AvlDict.fromList source)
                keys
            )


suiteOfSize : Int -> Benchmark
suiteOfSize size =
    describe
        (toString size)
        [ forValues "string" size toString
        , forValues "int" size identity
        , forValues "float" size toFloat
        , forValues "time" size (toFloat >> (*) Time.millisecond)
        , forValues "char" size Char.fromCode
        , forValues "tuple of int" size (\i -> ( i, i ))
        , forValues "list of int" size (\i -> [ i ])
        ]


main : BenchmarkProgram
main =
    program <| describe "modification" <| List.map suiteOfSize [ 1, 10, 100, 1000 ]
