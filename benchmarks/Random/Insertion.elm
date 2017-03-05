module Random.Insertion exposing (..)

import Benchmark exposing (..)
import Char
import Dict
import Dict.AVL as AvlDict
import Time exposing (Time)
import Random.Pcg as Random exposing (int, list, Seed)


seed : Seed
seed =
    Random.initialSeed 227852860


listOfSize : Seed -> Int -> List Int
listOfSize seed size =
    Random.step (list size (int 0 (3 * size))) seed
        |> Tuple.first


forValues : String -> Int -> (Int -> comparable) -> Benchmark
forValues name size keyer =
    let
        source =
            List.map
                (flip (,) ())
                (listOfSize seed size)
    in
        Benchmark.compare name
            (Benchmark.benchmark1 "Dict>" Dict.fromList source)
            (Benchmark.benchmark1 "Dict.AVL" AvlDict.fromList source)


suiteOfSize : Int -> Benchmark
suiteOfSize size =
    describe
        ("random insertion of size " ++ toString size)
        [ forValues "string" size toString
        , forValues "int" size identity
        , forValues "float" size toFloat
        , forValues "time" size (toFloat >> (*) Time.millisecond)
        , forValues "char" size Char.fromCode
        , forValues "tuple of int" size (\i -> ( i, i ))
        , forValues "list of int" size (\i -> [ i ])
        ]
