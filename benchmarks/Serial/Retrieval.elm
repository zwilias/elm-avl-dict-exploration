module Serial.Retrieval exposing (suiteOfSize, main)

import Benchmark.Runner exposing (BenchmarkProgram, program)
import Benchmark exposing (..)
import Char
import Dict as Dict
import Dict.AVL as AvlDict
import Time


forValues : String -> Int -> (Int -> comparable) -> Benchmark
forValues name size keyer =
    let
        keys =
            List.range 1 size
                |> List.map keyer

        source =
            List.map
                (flip (,) ())
                keys
    in
        Benchmark.compare name
            (Benchmark.benchmark2 "Dict"
                (\list dict -> List.map (\x -> Dict.get x dict) list)
                (keys)
                (Dict.fromList source)
            )
            (Benchmark.benchmark2 "Dict.AVL"
                (\list dict -> List.map (\x -> AvlDict.get x dict) list)
                (keys)
                (AvlDict.fromList source)
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
    program <| describe "cmp" <| List.map suiteOfSize [ 1, 10, 100, 1000 ]
