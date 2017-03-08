module Tree.AVL
    exposing
        ( Tree(Node, Empty)
        , singleton
        , empty
        , update
        , get
        , foldl
        , foldr
        , size
        , fromList
        , filter
        , heightDiff
        )


type Tree k v
    = Node Int k v (Tree k v) (Tree k v)
    | Empty



-- Basics: Creation


singleton : k -> v -> Tree k v
singleton key val =
    Node 1 key val empty empty


empty : Tree k v
empty =
    Empty



-- Basics: Insertion, deletion, member


{-| Tagger so the correct rebalancing operations can happen while bubbling up.
-}
type Flag comparable v
    = NeedRebalance (Tree comparable v)
    | NoNeed (Tree comparable v)
    | NoOp


update :
    comparable
    -> (Maybe v -> Maybe v)
    -> Tree comparable v
    -> Tree comparable v
update key alter tree =
    let
        getSmallest : Tree a b -> ( a, b )
        getSmallest tree =
            case tree of
                Empty ->
                    Debug.crash "can't"

                Node _ k v Empty _ ->
                    ( k, v )

                Node _ _ _ left _ ->
                    getSmallest left

        up :
            comparable
            -> (Maybe v -> Maybe v)
            -> Tree comparable v
            -> Flag comparable v
        up key alter tree =
            case tree of
                Empty ->
                    case alter Nothing of
                        Nothing ->
                            NoOp

                        Just value ->
                            singleton key value |> NeedRebalance

                Node level k v left right ->
                    case compare key k of
                        LT ->
                            case up key alter left of
                                NoNeed newLeft ->
                                    Node level k v newLeft right
                                        |> NoNeed

                                NeedRebalance newLeft ->
                                    build k v newLeft right
                                        |> balance
                                        |> NeedRebalance

                                NoOp ->
                                    NoOp

                        EQ ->
                            case alter <| Just v of
                                Nothing ->
                                    case ( left, right ) of
                                        ( Empty, _ ) ->
                                            right |> NeedRebalance

                                        ( _, Empty ) ->
                                            left |> NeedRebalance

                                        ( _, _ ) ->
                                            let
                                                ( skey, sval ) =
                                                    getSmallest right

                                                removeNext =
                                                    up
                                                        skey
                                                        (always Nothing)
                                                        right
                                            in
                                                case removeNext of
                                                    NoNeed newRight ->
                                                        build
                                                            skey
                                                            sval
                                                            left
                                                            newRight
                                                            |> NoNeed

                                                    NeedRebalance newRight ->
                                                        build
                                                            skey
                                                            sval
                                                            left
                                                            newRight
                                                            |> balance
                                                            |> NeedRebalance

                                                    NoOp ->
                                                        NoOp

                                Just value ->
                                    if value == v then
                                        NoOp
                                    else
                                        Node level key value left right
                                            |> NoNeed

                        GT ->
                            case up key alter right of
                                NoNeed newRight ->
                                    Node level k v left newRight
                                        |> NoNeed

                                NeedRebalance newRight ->
                                    build k v left newRight
                                        |> balance
                                        |> NeedRebalance

                                NoOp ->
                                    NoOp
    in
        case up key alter tree of
            NoNeed tree ->
                tree

            NeedRebalance tree ->
                tree

            NoOp ->
                tree


get : comparable -> Tree comparable v -> Maybe v
get key tree =
    case tree of
        Empty ->
            Nothing

        Node _ head value left right ->
            case compare key head of
                LT ->
                    get key left

                GT ->
                    get key right

                EQ ->
                    Just value



-- Argument reposition helpers


flip2nd : (a -> b -> c -> d) -> a -> c -> b -> d
flip2nd op a c b =
    op a b c



-- Folds


foldl : (k -> v -> a -> a) -> a -> Tree k v -> a
foldl op acc tree =
    case tree of
        Empty ->
            acc

        Node _ key val left right ->
            foldl op acc left
                |> op key val
                |> flip2nd foldl op right


foldr : (k -> v -> a -> a) -> a -> Tree k v -> a
foldr op acc tree =
    case tree of
        Empty ->
            acc

        Node _ key val left right ->
            foldr op acc right
                |> op key val
                |> flip2nd foldr op left



-- Internal, implementation-specific operations


{-| Create a new set with a given element and a predecided lefthand and
righthand value. If both left and right are Empty, returns a Singleton.

Note that this constructor does *not* use insertion internally, and - hence -
**does not sustain the AVL invariant**.

```
import Tree.Self as Tree exposing (Tree)

customSingleton : Int -> Tree Int
customSingleton val =
    Tree.tree val Tree.empty Tree.empty


customSingleton 1 == < 1 . . >
```
-}
build : k -> v -> Tree k v -> Tree k v -> Tree k v
build key value left right =
    Node
        (max
            (height left)
            (height right)
            |> (+) 1
        )
        key
        value
        left
        right


{-| The height of a set is something baked right into the Tree, and is important
for balancing the internal tree. A properly balanced tree will have a maximal
height-difference between branches of |1|.

```
import Tree.Self as Tree exposing (Tree)


Tree.height Tree.empty == 0
Tree.height Tree.singleton 0 == 1

Tree.fromList [ 1, 2, 3 ]
    |> Tree.height == 2
```
-}
height : Tree k v -> Int
height set =
    case set of
        Empty ->
            0

        Node height _ _ _ _ ->
            height


{-| Rotate a tree to the left (for balancing).
-}
rotateLeft : Tree k v -> Tree k v
rotateLeft set =
    case set of
        Node level root rootVal less (Node rLevel pivot pivotVal between greater) ->
            build pivot pivotVal (build root rootVal less between) greater

        _ ->
            set


{-| Inversely, rotate a tree to the right (for balancing).
-}
rotateRight : Tree k v -> Tree k v
rotateRight set =
    case set of
        Node level root rootVal (Node lLevel pivot pivotVal less between) greater ->
            build pivot pivotVal less (build root rootVal between greater)

        _ ->
            set


{-| Calculate the difference in height between our left and right branches.

A *valid* AVL tree has a maximal height difference of |1| over its branches,
which allows checking if a random element is in the tree in `O (log n)`.
-}
heightDiff : Tree k v -> Int
heightDiff set =
    case set of
        Empty ->
            0

        Node _ _ _ left right ->
            height right - height left


{-| Rebalances a tree (if it is, in fact, unbalanced).

If a tree becomes unbalanced by |2|, this restores the balance by rotating and
-- if a child is unbalanced in the opposite direction -- rotating the child in
the opposite direction.

For more information on how this works, please refer to [Brian Hick's excellent
series](https://www.brianthicks.com/post/2016/11/27/functional-sets-part-3-balancing/)
on implementing AVL trees in Elm.
-}
balance : Tree comparable v -> Tree comparable v
balance set =
    case set of
        Empty ->
            set

        Node level key value left right ->
            let
                setDiff =
                    heightDiff set
            in
                if setDiff == -2 then
                    if heightDiff left == 1 then
                        {- left leaning tree with right-leaning left subtree.
                           Rotate left, then right.
                        -}
                        Node (level) key value (rotateLeft left) right
                            |> rotateRight
                    else
                        -- left leaning tree, generally. Rotate right.
                        rotateRight set
                else if setDiff == 2 then
                    if heightDiff right == -1 then
                        {- right leaning tree with left-leaning right subtree.
                           Rotate right, then left.
                        -}
                        Node level key value left (rotateRight right)
                            |> rotateLeft
                    else
                        -- right leaning tree, generally. Rotate left.
                        rotateLeft set
                else
                    -- diff is -1, 0, or 1. Already balanced!
                    set



-- Helpers


{-| Convert an association list into a dictionary.
-}
fromList : List ( comparable, v ) -> Tree comparable v
fromList =
    List.foldl
        (\( key, value ) dict -> update key (always <| Just value) dict)
        empty


{-| Determine the number of key-value pairs in the dictionary.
-}
size : Tree k v -> Int
size =
    foldl (\_ _ -> (+) 1) 0


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (comparable -> v -> Bool) -> Tree comparable v -> Tree comparable v
filter predicate =
    foldl
        (\k v ->
            if predicate k v then
                update k (always <| Just v)
            else
                identity
        )
        empty
