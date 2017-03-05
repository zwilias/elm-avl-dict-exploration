module Serial.Insertion exposing (suiteOfSize)

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
                (\n -> ( keyer n, () ))
                (List.range 1 size)
    in
        Benchmark.compare (name)
            (Benchmark.benchmark1 "Dict" Dict.fromList source)
            (Benchmark.benchmark1 "Dict.AVL" AvlDict.fromList source)


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
