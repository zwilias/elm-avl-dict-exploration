module Random.Deletion exposing (suiteOfSize, main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Char
import Dict
import Dict.AVL as AvlDict
import Random.Pcg as Random exposing (int, list, Seed)
import Time


( seed, otherSeed ) =
    Random.initialSeed 227852860
        |> Random.step Random.independentSeed


listOfSize : Seed -> Int -> List Int
listOfSize seed size =
    Random.step (list size (int 0 (3 * size))) seed
        |> Tuple.first


forValues : String -> Int -> (Int -> comparable) -> Benchmark
forValues name size keyer =
    let
        source =
            List.map
                (\n -> ( keyer n, () ))
                (listOfSize seed size)

        toRemove =
            List.map
                keyer
                (listOfSize otherSeed size)
    in
        Benchmark.compare name
            (Benchmark.benchmark2 "Dict" (List.foldl Dict.remove) (Dict.fromList source) (toRemove))
            (Benchmark.benchmark2 "Dict.AVL" (List.foldl AvlDict.remove) (AvlDict.fromList source) (toRemove))


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
