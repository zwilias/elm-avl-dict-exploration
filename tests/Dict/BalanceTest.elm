module Dict.BalanceTest exposing (..)

import Test exposing (..)
import Expect
import Dict.AVL as Dict exposing (..)
import Fuzz exposing (list, int)
import Util


tests : Test
tests =
    describe "AVL tree"
        [ describe "Basic sanity"
            [ test "Empty" <|
                \() ->
                    Dict.empty
                        |> Dict.size
                        |> Expect.equal 0
            , fuzz int "Singleton" <|
                \item ->
                    Dict.singleton item ()
                        |> Expect.all
                            [ Dict.member item >> Expect.true "Singleton should contain value"
                            , Dict.size >> Expect.equal 1
                            ]
            , fuzz (list int) "Member" <|
                \items ->
                    items
                        |> List.map (flip (,) ())
                        |> Dict.fromList
                        |> flip Dict.member
                        |> flip List.all items
                        |> Expect.true "Expect all items to be present in list"
            ]
        , describe "Invariants for AVL tree"
            [ invariant
                "BST"
                Dict.isValidBst
            , invariant
                "The absolute balancefactor at each node is at most 1."
                Dict.isValidAvl
            ]
        ]



-- invariants


invariant : String -> (Dict Int () -> Bool) -> Test
invariant desc checker =
    describe desc
        [ testInsertion checker
        , testRemoval checker
        ]


testInsertion : (Dict Int () -> Bool) -> Test
testInsertion checker =
    fuzz (list int) "Invariant holds during insertions" <|
        \members ->
            members
                |> List.map (flip (,) ())
                |> Dict.fromList
                |> checker
                |> Expect.true "Invariant did not hold"


testRemoval : (Dict Int () -> Bool) -> Test
testRemoval checker =
    fuzz Util.listAndSublist "Invariant holds during removal" <|
        \( members, remove ) ->
            let
                removeItems : Dict Int () -> Dict Int ()
                removeItems tree =
                    remove
                        |> List.foldl Dict.remove tree
            in
                members
                    |> List.map (flip (,) ())
                    |> Dict.fromList
                    |> removeItems
                    |> checker
                    |> Expect.true "Invariant did not hold"
