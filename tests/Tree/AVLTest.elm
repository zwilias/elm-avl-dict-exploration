module Tree.AVLTest exposing (..)

import Test exposing (..)
import Expect
import Tree.AVL as Tree exposing (..)
import Fuzz exposing (list, int)
import Util


all : Test
all =
    describe "AVL tree"
        [ describe "Basic sanity"
            [ test "Empty" <|
                \() ->
                    Tree.empty
                        |> Tree.size
                        |> Expect.equal 0
            , fuzz int "Singleton" <|
                \item ->
                    Tree.singleton item ()
                        |> Expect.all
                            [ treemember item >> Expect.true "Singleton should contain value"
                            , Tree.size >> Expect.equal 1
                            ]
            , fuzz (list int) "Member" <|
                \items ->
                    items
                        |> List.map (flip (,) ())
                        |> Tree.fromList
                        |> flip treemember
                        |> flip List.all items
                        |> Expect.true "Expect all items to be present in list"
            ]
        , describe "Invariants for AVL tree"
            [ invariant
                "BST"
                checker0
            , invariant
                "The absolute balancefactor at each node is at most 1."
                checker1
            ]
        ]



-- BST invariant


checker0 : Tree comparable a -> Bool
checker0 tree =
    case tree of
        Empty ->
            True

        Tree.Node _ key _ left right ->
            allInTree (\k v -> k < key) left
                && allInTree (\k v -> k > key) right
                && checker0 left
                && checker0 right


allInTree : (comparable -> a -> Bool) -> Tree comparable a -> Bool
allInTree predicate tree =
    Tree.filter (\k v -> not (predicate k v)) tree
        |> \tree -> Tree.size tree == 0



-- Modification helpers


treemember : comparable -> Tree comparable v -> Bool
treemember key tree =
    Tree.get key tree /= Nothing


treeremove : comparable -> Tree comparable v -> Tree comparable v
treeremove key =
    Tree.update key (always Nothing)



-- invariants


{-| The level of every leaf node is one.
-}
checker1 : Tree comparable a -> Bool
checker1 tree =
    case tree of
        Empty ->
            True

        Node _ _ _ left right ->
            (Tree.heightDiff tree |> abs)
                <= 1
                && checker1 left
                && checker1 right



-- Invariant checker


invariant : String -> (Tree Int () -> Bool) -> Test
invariant desc checker =
    describe desc
        [ testInsertion checker
        , testRemoval checker
        ]


testInsertion : (Tree Int () -> Bool) -> Test
testInsertion checker =
    fuzz (list int) "Invariant holds during insertions" <|
        \members ->
            members
                |> List.map (flip (,) ())
                |> Tree.fromList
                |> checker
                |> Expect.true "Invariant did not hold"


testRemoval : (Tree Int () -> Bool) -> Test
testRemoval checker =
    fuzz Util.listAndSublist "Invariant holds during removal" <|
        \( members, remove ) ->
            let
                removeItems : Tree Int () -> Tree Int ()
                removeItems tree =
                    remove
                        |> List.foldl treeremove tree
            in
                members
                    |> List.map (flip (,) ())
                    |> Tree.fromList
                    |> removeItems
                    |> checker
                    |> Expect.true "Invariant did not hold"
