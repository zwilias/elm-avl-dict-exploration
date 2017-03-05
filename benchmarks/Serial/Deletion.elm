module Serial.Deletion exposing (..)

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
                (flip (,) ())
                keys

        keys =
            List.range 1 size
                |> List.map keyer
    in
        Benchmark.compare name
            (Benchmark.benchmark2 "Dict" (List.foldl Dict.remove) (Dict.fromList source) keys)
            (Benchmark.benchmark2 "Dict.AVL" (List.foldl AvlDict.remove) (AvlDict.fromList source) keys)


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
