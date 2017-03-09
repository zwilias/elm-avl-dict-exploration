module Serial.Enumeration exposing (suiteOfSize)

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
            (Benchmark.benchmark1 "Dict"
                (Dict.toList)
                (Dict.fromList source)
            )
            (Benchmark.benchmark1 "Dict.AVL"
                (AvlDict.toList)
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
