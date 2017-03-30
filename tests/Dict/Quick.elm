module Dict.Quick exposing (tests)

import Test exposing (..)
import Fuzz exposing (list, int, tuple, constant)
import Expect
import Dict.AVL as Dict


reversePairs : ( comparable, a ) -> ( comparable, a ) -> Order
reversePairs ( a, _ ) ( b, _ ) =
    compare b a


uniqueAndReverse : List ( a, b ) -> List ( a, b )
uniqueAndReverse list =
    case list of
        [] ->
            []

        x :: xs ->
            let
                ( first, rest ) =
                    List.foldl
                        (\( ck, kv ) ( ( pk, pv ), res ) ->
                            if (ck == pk) then
                                ( ( pk, pv ), res )
                            else
                                ( ( ck, kv ), ( pk, pv ) :: res )
                        )
                        ( x, [] )
                        xs
            in
                first :: rest


tests : Test
tests =
    describe "quick tests"
        [ fuzz (list (tuple ( int, constant () ))) "reverse pairs" <|
            \xs ->
                List.sortWith reversePairs xs
                    |> Expect.equalLists
                        (List.sortBy Tuple.first xs |> List.reverse)
        , test "reverses" <|
            \() ->
                uniqueAndReverse [ ( 3, () ), ( 2, () ), ( 1, () ) ]
                    |> Expect.equalLists [ ( 1, () ), ( 2, () ), ( 3, () ) ]
        , test "removes duplicates" <|
            \() ->
                uniqueAndReverse [ ( 3, () ), ( 3, () ), ( 2, () ), ( 2, () ), ( 1, () ), ( 1, () ) ]
                    |> Expect.equalLists [ ( 1, () ), ( 2, () ), ( 3, () ) ]
        , test "removes sequential dupes" <|
            \() ->
                uniqueAndReverse
                    [ ( 3, "keep" )
                    , ( 3, "dupe" )
                    , ( 2, "keep" )
                    , ( 2, "dupe" )
                    , ( 1, "keep" )
                    , ( 1, "dupe" )
                    ]
                    |> Expect.equalLists
                        [ ( 1, "keep" )
                        , ( 2, "keep" )
                        , ( 3, "keep" )
                        ]
        , fuzz (list (tuple ( int, constant () ))) "create dics" <|
            \xs ->
                Dict.fromList xs
                    |> Dict.eq (Dict.fromList2 xs)
                    |> Expect.true "equal dicts"
        ]
