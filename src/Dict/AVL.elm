module Dict.AVL
    exposing
        ( Dict
        , empty
        , singleton
        , insert
        , update
        , isEmpty
        , get
        , remove
        , member
        , size
        , filter
        , partition
        , foldl
        , foldr
        , map
        , union
        , intersect
        , diff
        , merge
        , keys
        , values
        , toList
        , fromList
        , eq
        )

{-| Dict.AVL is an alternative Dict implementation backed by an AVL tree rather
than a red black tree. It offers much better write performance at the cost of
marginally worse deletion performance and about on-par retrieval performance.

Its API is exactly the same as that of the core Dict, with *one* difference:
checking for equality. With core's Dict, there is a special case in the `==`
operator, which we can't use for our Dict. As such, we expose an `eq` function
that can be used to check for equality of 2 Dicts.

---

A dictionary mapping unique keys to values. The keys can be any comparable
type. This includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or
lists of comparable types.

Insert, remove, and query operations all take *O(log n)* time.

# Dictionaries
@docs Dict

# Build
@docs empty, singleton, insert, update, remove

# Query
@docs isEmpty, member, get, size, eq

# Lists
@docs keys, values, toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition

# Combine
@docs union, intersect, diff, merge

-}


{-| A dictionary of keys and values. So a `(Dict String User)` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.
-}
type Dict k v
    = Empty
    | Node Int k v (Dict k v) (Dict k v)


{-| Create an empty dictionary.
-}
empty : Dict k v
empty =
    Empty


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : comparable -> Dict comparable v -> Maybe v
get key dict =
    case dict of
        Empty ->
            Nothing

        Node _ head value left right ->
            case compare key head of
                EQ ->
                    Just value

                LT ->
                    get key left

                GT ->
                    get key right


{-| Check if two dicts are equal.

Core's Dict is special cased in the `==` operator. Since the only way to get
that same special casing is by using the exact same internal layout for the tree
(so that it is compatible with `_elm_lang$core$Dict$toList`), it seems
preferable to expose an `eq` function instead and *hope* people don't forget to
use it.

    dictOne = fromList [ (1, 1), (2, 2) ]
    dictTwo = fromList [ (1, 1), (2, 2) ]
    dictThree = fromList [ (1, 1) ]

    eq dictOne dictTwo == true
    eq dictOne dictThree == False

-}
eq : Dict comparable v -> Dict comparable v -> Bool
eq left right =
    (toList left) == (toList right)


{-| Determine if a key is in a dictionary.
-}
member : comparable -> Dict comparable v -> Bool
member key dict =
    get key dict /= Nothing


{-| Determine the number of key-value pairs in the dictionary.
-}
size : Dict k v -> Int
size dict =
    foldl (\_ _ acc -> acc + 1) 0 dict


{-| Determine if a dictionary is empty.

    isEmpty empty == True
-}
isEmpty : Dict k v -> Bool
isEmpty dict =
    dict == empty


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : comparable -> v -> Dict comparable v -> Dict comparable v
insert key value dict =
    update key (\_ -> Just value) dict


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : comparable -> Dict comparable v -> Dict comparable v
remove key dict =
    update key (\_ -> Nothing) dict


type Flag comparable v
    = NeedRebalance (Dict comparable v)
    | NoNeed (Dict comparable v)
    | NoOp


{-| Update the value of a dictionary for a specific key with a given function.
-}
update :
    comparable
    -> (Maybe v -> Maybe v)
    -> Dict comparable v
    -> Dict comparable v
update key alter dict =
    let
        getSmallest : Dict a b -> ( a, b )
        getSmallest dict =
            case dict of
                Empty ->
                    Debug.crash "can't"

                Node _ k v Empty _ ->
                    ( k, v )

                Node _ _ _ left _ ->
                    getSmallest left

        up :
            comparable
            -> (Maybe v -> Maybe v)
            -> Dict comparable v
            -> Flag comparable v
        up key alter dict =
            case dict of
                Empty ->
                    case alter Nothing of
                        Nothing ->
                            NoOp

                        Just value ->
                            NeedRebalance (singleton key value)

                Node level k v left right ->
                    case compare key k of
                        LT ->
                            case up key alter left of
                                NoNeed newLeft ->
                                    NoNeed (Node level k v newLeft right)

                                NeedRebalance newLeft ->
                                    NeedRebalance
                                        (balance (build k v newLeft right))

                                NoOp ->
                                    NoOp

                        EQ ->
                            case alter <| Just v of
                                Nothing ->
                                    case ( left, right ) of
                                        ( Empty, _ ) ->
                                            NeedRebalance right

                                        ( _, Empty ) ->
                                            NeedRebalance left

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
                                                        NoNeed
                                                            (build
                                                                skey
                                                                sval
                                                                left
                                                                newRight
                                                            )

                                                    NeedRebalance newRight ->
                                                        NeedRebalance
                                                            (balance
                                                                (build
                                                                    skey
                                                                    sval
                                                                    left
                                                                    newRight
                                                                )
                                                            )

                                                    NoOp ->
                                                        NoOp

                                Just value ->
                                    if value == v then
                                        NoOp
                                    else
                                        NoNeed (Node level key value left right)

                        GT ->
                            case up key alter right of
                                NoNeed newRight ->
                                    NoNeed (Node level k v left newRight)

                                NeedRebalance newRight ->
                                    NeedRebalance
                                        (balance (build k v left newRight))

                                NoOp ->
                                    NoOp
    in
        case up key alter dict of
            NoNeed dict ->
                dict

            NeedRebalance dict ->
                dict

            NoOp ->
                dict


{-| Create a dictionary with one key-value pair.
-}
singleton : k -> v -> Dict k v
singleton key val =
    Node 1 key val empty empty



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : Dict comparable v -> Dict comparable v -> Dict comparable v
union left right =
    foldl insert right left


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : Dict comparable v -> Dict comparable v -> Dict comparable v
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : Dict comparable v -> Dict comparable v -> Dict comparable v
diff t1 t2 =
    filter (\k _ -> not <| member k t2) t1


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

    1. Only in the left dictionary.
    2. In both dictionaries.
    3. Only in the right dictionary.

You then traverse all the keys from lowest to highest, building up whatever
you want.
-}
merge :
    (comparable -> a -> result -> result)
    -> (comparable -> a -> b -> result -> result)
    -> (comparable -> b -> result -> result)
    -> Dict comparable a
    -> Dict comparable b
    -> result
    -> result
merge leftStep bothStep rightStep leftDict rightDict initialResult =
    let
        stepState rKey rValue ( list, result ) =
            case list of
                [] ->
                    ( list, rightStep rKey rValue result )

                ( lKey, lValue ) :: rest ->
                    if lKey < rKey then
                        stepState
                            rKey
                            rValue
                            ( rest, leftStep lKey lValue result )
                    else if lKey > rKey then
                        ( list, rightStep rKey rValue result )
                    else
                        ( rest, bothStep lKey lValue rValue result )

        ( leftovers, intermediateResult ) =
            foldl stepState ( toList leftDict, initialResult ) rightDict
    in
        List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers



-- TRANSFORM


{-| Apply a function to all values in a dictionary.
-}
map : (comparable -> a -> b) -> Dict comparable a -> Dict comparable b
map f =
    foldl
        (\key val dict -> insert key (f key val) dict)
        empty


{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key.
-}
foldl : (k -> v -> a -> a) -> a -> Dict k v -> a
foldl op acc dict =
    case dict of
        Empty ->
            acc

        Node _ key val left right ->
            foldl op (op key val (foldl op acc left)) right


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key.
-}
foldr : (k -> v -> a -> a) -> a -> Dict k v -> a
foldr op acc dict =
    case dict of
        Empty ->
            acc

        Node _ key val left right ->
            foldr op (op key val (foldr op acc right)) left


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (comparable -> v -> Bool) -> Dict comparable v -> Dict comparable v
filter predicate =
    foldl
        (\k v ->
            if predicate k v then
                insert k v
            else
                identity
        )
        empty


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition :
    (comparable -> v -> Bool)
    -> Dict comparable v
    -> ( Dict comparable v, Dict comparable v )
partition predicate =
    foldl
        (\k v ->
            if predicate k v then
                Tuple.mapFirst (insert k v)
            else
                Tuple.mapSecond (insert k v)
        )
        ( empty, empty )



-- LISTS


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [(0,"Alice"),(1,"Bob")]) == [0,1]
-}
keys : Dict comparable v -> List comparable
keys dict =
    foldr (\key _ keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]
-}
values : Dict comparable v -> List v
values dict =
    foldr (always (::)) [] dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : Dict comparable v -> List ( comparable, v )
toList dict =
    foldr (\key value list -> ( key, value ) :: list) [] dict


{-| Convert an association list into a dictionary.
-}
fromList : List ( comparable, v ) -> Dict comparable v
fromList assocs =
    List.foldl
        (\( key, value ) dict -> update key (\_ -> Just value) dict)
        empty
        assocs



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
build : k -> v -> Dict k v -> Dict k v -> Dict k v
build key value left right =
    Node
        ((max
            (height left)
            (height right)
         )
            + 1
        )
        key
        value
        left
        right


{-| The height of a set is something baked right into the Tree, and is important
for balancing the internal tree. A properly balanced tree will have a maximal
height-difference between branches of |1|.
-}
height : Dict k v -> Int
height dict =
    case dict of
        Empty ->
            0

        Node height _ _ _ _ ->
            height


{-| Rotate a tree to the left (for balancing).
-}
rotateLeft : Dict k v -> Dict k v
rotateLeft dict =
    case dict of
        Node _ root rootVal less (Node _ pivot pivotVal between greater) ->
            build pivot pivotVal (build root rootVal less between) greater

        _ ->
            dict


{-| Inversely, rotate a tree to the right (for balancing).
-}
rotateRight : Dict k v -> Dict k v
rotateRight dict =
    case dict of
        Node _ root rootVal (Node _ pivot pivotVal less between) greater ->
            build pivot pivotVal less (build root rootVal between greater)

        _ ->
            dict


{-| Calculate the difference in height between our left and right branches.

A *valid* AVL tree has a maximal height difference of |1| over its branches,
which allows checking if a random element is in the tree in `O (log n)`.
-}
heightDiff : Dict k v -> Int
heightDiff dict =
    case dict of
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
balance : Dict comparable v -> Dict comparable v
balance dict =
    case dict of
        Empty ->
            dict

        Node level key value left right ->
            let
                setDiff =
                    heightDiff dict
            in
                if setDiff == -2 then
                    if heightDiff left == 1 then
                        {- left leaning tree with right-leaning left subtree.
                           Rotate left, then right.
                        -}
                        rotateRight
                            (Node level key value (rotateLeft left) right)
                    else
                        -- left leaning tree, generally. Rotate right.
                        rotateRight dict
                else if setDiff == 2 then
                    if heightDiff right == -1 then
                        {- right leaning tree with left-leaning right subtree.
                           Rotate right, then left.
                        -}
                        rotateLeft
                            (Node level key value left (rotateRight right))
                    else
                        -- right leaning tree, generally. Rotate left.
                        rotateLeft dict
                else
                    -- diff is -1, 0, or 1. Already balanced!
                    dict
