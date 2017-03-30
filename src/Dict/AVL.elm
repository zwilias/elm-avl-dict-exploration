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
        , isValidAvl
        , isValidBst
        , fromList2
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

# Checks

In addition to the general API described here, we also expose two function that
can be used to check the internal consistency of the AVL tree backing the Dict.
These should *always* return True, which is what our test suite is testing.
@docs isValidBst, isValidAvl


# Other
@docs fromList2
-}

{-
   import Benchmark exposing (Benchmark, benchmark1, describe)
   import Benchmark.Runner exposing (BenchmarkProgram, program)


   listOfSize : Int -> (Int -> comparable) -> List ( comparable, () )
   listOfSize n keyer =
       List.range 1 n
           -- |> List.reverse
           |> List.map (keyer >> flip (,) ())


   createCompare : Int -> Benchmark
   createCompare size =
       let
           l =
               listOfSize size (\x -> ( x, x ))
       in
           Benchmark.compare ("size " ++ (toString size))
               (benchmark1 "foldl" fromList l)
               (benchmark1 "rec split" fromList2 l)


   main : BenchmarkProgram
   main =
       program <|
           describe "fromlists" <|
               List.map createCompare [ 1, 10, 100, 500, 1000, 10000 ]
-}


{-| A dictionary of keys and values. So a `(Dict String User)` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.
-}
type Dict k v
    = Empty
    | Node Int k v (Dict k v) (Dict k v)


{-| Create an empty dictionary.

    >>> empty |> toList
    []

    >>> insert 0 "value" empty |> toList
    [ ( 0, "value" )]
-}
empty : Dict k v
empty =
    Empty


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    >>> fromList [ ( "Tom", "Cat" ), ( "Jerry", "Mouse" ) ]
    ... |> get "Tom"
    Just "Cat"

    >>> fromList [ ( "Tom", "Cat" ), ( "Jerry", "Mouse" ) ]
    ... |> get "Jerry"
    Just "Mouse"

    >>> fromList [ ( "Tom", "Cat" ), ( "Jerry", "Mouse" ) ]
    ... |> get "Spike"
    Nothing

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

    >>> fromList [ ( 1, 1 ), ( 2, 2 ) ]
    ... |> eq (fromList [ ( 2, 2 ), ( 1, 1 ) ])
    True


    >>> fromList [ ( 1, 1 ), ( 2, 2 ) ]
    ... |> eq empty
    False

-}
eq : Dict comparable v -> Dict comparable v -> Bool
eq left right =
    toList left == toList right


{-| Determine if a key is in a dictionary.

    >>> fromList [ ( 1, 1 ), ( 2, 2 ) ]
    ... |> member 1
    True

    >>> fromList [ ( 1, 1 ), ( 2, 2 ) ]
    ... |> member 3
    False
-}
member : comparable -> Dict comparable v -> Bool
member key dict =
    get key dict /= Nothing


{-| Determine the number of key-value pairs in the dictionary.

    >>> fromList [ ( 1, 1 ), ( 2, 2 ) ]
    ... |> size
    2

    >>> size empty
    0
-}
size : Dict k v -> Int
size dict =
    foldl (\_ _ acc -> acc + 1) 0 dict


{-| Determine if a dictionary is empty.

    >>> isEmpty empty
    True

    >>> singleton 1 "value"
    ... |> isEmpty
    False
-}
isEmpty : Dict k v -> Bool
isEmpty dict =
    dict == empty


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.

    >>> insert 1 "val" empty
    ... |> toList
    [ ( 1, "val" ) ]

    >>> singleton 1 "original"
    ... |> insert 1 "updated"
    ... |> toList
    [ ( 1, "updated" ) ]
-}
insert : comparable -> v -> Dict comparable v -> Dict comparable v
insert key value dict =
    update key (\_ -> Just value) dict


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.

    >>> singleton 1 "value"
    ... |> remove 1
    ... |> toList
    []

    >>> singleton 1 "value"
    ... |> remove 2
    ... |> toList
    [ ( 1, "value" ) ]
-}
remove : comparable -> Dict comparable v -> Dict comparable v
remove key dict =
    update key (\_ -> Nothing) dict


{-| Internal type used to unwind the stack correctly after an update. An
operation can result in one of 3 things:

- The tree needs to be rebalanced while unwinding (insert/remove)
- The tree remains balanced (update)
- The tree didn't change, so we return the original tree (no op)
-}
type Flag comparable v
    = NeedRebalance (Dict comparable v)
    | NoNeed (Dict comparable v)


{-| Internal helper, used for handling internal delete operations. It uses 2
"subfunctions" to first retrieve the smallest key/value pair in the tree, then
removes that, and returns both the retrieved pair as well as the resulting tree.

This can then be used to handle internal deletes much more easily by always
opting to remove a leaf-node, and replacing the node that was actually supposed
to be deleted by the next-smallest value.

Fetching the value first, then removing it, rather than doing the entire thing
in one step, is done to maintain performance - the overhead of constructing a
larger tuple *and* not being able to do this in a tail-call optimized manner,
combined with the fact that trees don't grow very deep means we don't actually
gain anything from attempting to do so.
-}
removeSmallest :
    Dict comparable v
    -> ( ( comparable, v ), Dict comparable v )
removeSmallest subTree =
    let
        getSmallest : Dict comparable v -> ( comparable, v )
        getSmallest inDict =
            case inDict of
                Node _ sKey sVal Empty _ ->
                    ( sKey, sVal )

                Node _ _ _ left _ ->
                    getSmallest left

                Empty ->
                    Debug.crash "unreachable"

        doRemove : Dict comparable v -> Dict comparable v
        doRemove fromDict =
            case fromDict of
                Node _ _ _ Empty right ->
                    right

                Node _ k v left right ->
                    balance (build k v (doRemove left) right)

                Empty ->
                    Debug.crash "unreachable"
    in
        ( getSmallest subTree, doRemove subTree )


{-| Update the value of a dictionary for a specific key with a given function.

    >>> singleton 1 "original"
    ... |> update 1 (Maybe.map String.toUpper)
    ... |> toList
    [ ( 1, "ORIGINAL" ) ]

    >>> update 1 (always <| Just "value") empty
    ... |> toList
    [ ( 1, "value" ) ]

    >>> singleton 1 "original"
    ... |> update 1 (always Nothing)
    ... |> toList
    []
-}
update :
    comparable
    -> (Maybe v -> Maybe v)
    -> Dict comparable v
    -> Dict comparable v
update key alter input =
    let
        up :
            Dict comparable v
            -> Flag comparable v
        up dict =
            case dict of
                Empty ->
                    case alter Nothing of
                        Nothing ->
                            NoNeed
                                dict

                        Just value ->
                            NeedRebalance
                                (singleton key value)

                Node level k v left right ->
                    case compare key k of
                        LT ->
                            case up left of
                                NoNeed newLeft ->
                                    NoNeed
                                        (Node level k v newLeft right)

                                NeedRebalance newLeft ->
                                    NeedRebalance
                                        (balance (build k v newLeft right))

                        EQ ->
                            case alter (Just v) of
                                Nothing ->
                                    case ( left, right ) of
                                        ( Empty, _ ) ->
                                            NeedRebalance right

                                        ( _, Empty ) ->
                                            NeedRebalance left

                                        ( _, _ ) ->
                                            let
                                                ( ( nKey, nVal ), nRight ) =
                                                    removeSmallest right
                                            in
                                                NeedRebalance
                                                    (balance
                                                        (build
                                                            nKey
                                                            nVal
                                                            left
                                                            nRight
                                                        )
                                                    )

                                Just value ->
                                    NoNeed
                                        (Node level k value left right)

                        GT ->
                            case up right of
                                NoNeed newRight ->
                                    NoNeed
                                        (Node level k v left newRight)

                                NeedRebalance newRight ->
                                    NeedRebalance
                                        (balance (build k v left newRight))
    in
        case up input of
            NoNeed result ->
                result

            NeedRebalance result ->
                result


{-| Create a dictionary with one key-value pair.

    >>> singleton 1 "val"
    ... |> toList
    [ (1, "val") ]

    >>> fromList [ ( "key", "val" ) ]
    ... |> eq (singleton "key" "val")
    True
-}
singleton : k -> v -> Dict k v
singleton key val =
    Node 1 key val empty empty



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.

    >>> union
    ...    (fromList [ ( 1, "a" ), ( 3, "b" ), ( 5, "c" ) ])
    ...    (fromList [ ( 2, "x" ), ( 3, "y" ), ( 4, "z" ) ])
    ... |> toList
    [ ( 1, "a" )
    , ( 2, "x" )
    , ( 3, "b" )
    , ( 4, "z" )
    , ( 5, "c" )
    ]
-}
union : Dict comparable v -> Dict comparable v -> Dict comparable v
union left right =
    foldl insert right left


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.

    >>> intersect
    ...    (fromList [ ( 1, "a" ), ( 3, "b" ), ( 5, "c" ) ])
    ...    (fromList [ ( 2, "x" ), ( 3, "y" ), ( 4, "z" ) ])
    ... |> toList
    [ ( 3, "b" ) ]
-}
intersect : Dict comparable v -> Dict comparable v -> Dict comparable v
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second dictionary.

    >>> diff
    ...    (fromList [ ( 1, "a" ), ( 3, "b" ), ( 5, "c" ) ])
    ...    (fromList [ ( 2, "x" ), ( 3, "y" ), ( 4, "z" ) ])
    ... |> toList
    [ ( 1, "a" )
    , ( 5, "c" )
    ]
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

    >>> merge
    ...     (\k v ( inLeft, inBoth, inRight ) ->
    ...         ( ( k, v ) :: inLeft
    ...         , inBoth
    ...         , inRight) )
    ...     (\k vLeft vRight ( inLeft, inBoth, inRight ) ->
    ...         ( inLeft
    ...         , ( k, vLeft, vRight ) :: inBoth
    ...         , inRight) )
    ...     (\k v ( inLeft, inBoth, inRight ) ->
    ...         ( inLeft
    ...         , inBoth
    ...         , ( k, v ) :: inRight) )
    ...     (fromList [ ( 1, "a" ), ( 3, "b" ), ( 5, "c" ) ])
    ...     (fromList [ ( 2, "x" ), ( 3, "y" ), ( 4, "z" ) ])
    ...     ( [], [], [] )
    ( [ ( 5, "c" ), ( 1, "a" ) ]
    , [ ( 3, "b", "y" ) ]
    , [ ( 4, "z" ), ( 2, "x" ) ]
    )
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

    >>> singleton "key" "value"
    ... |> map (\k v -> String.toUpper v)
    ... |> toList
    [ ( "key", "VALUE" ) ]
-}
map : (comparable -> a -> b) -> Dict comparable a -> Dict comparable b
map f tree =
    case tree of
        Empty ->
            Empty

        Node height key value left right ->
            Node height key (f key value) (map f left) (map f right)


{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key.

    >>> fromList [ ( 1, "first" ), ( 2, "second" ) ]
    ... |> foldl (\key value acc -> (key, value) :: acc) []
    [ ( 2, "second" )
    , ( 1, "first" )
    ]
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

    >>> fromList [ ( 1, "first" ), ( 2, "second" ) ]
    ... |> foldr (\key value acc -> (key, value) :: acc) []
    [ ( 1, "first" )
    , ( 2, "second" )
    ]
-}
foldr : (k -> v -> a -> a) -> a -> Dict k v -> a
foldr op acc dict =
    case dict of
        Empty ->
            acc

        Node _ key val left right ->
            foldr op (op key val (foldr op acc right)) left


foldr2 : (k -> v -> a -> a) -> a -> Dict k v -> a
foldr2 op acc dict =
    foldrHelper op acc dict None


type Cont k v
    = None
    | Cont k v (Dict k v) (Cont k v)


foldrHelper : (k -> v -> a -> a) -> a -> Dict k v -> Cont k v -> a
foldrHelper op acc dict cont =
    case dict of
        Empty ->
            case cont of
                None ->
                    acc

                Cont key val next tail ->
                    foldrHelper op (op key val acc) next tail

        Node _ key val left right ->
            foldrHelper
                op
                acc
                right
                (Cont key val left cont)


foldr3 : (k -> v -> a -> a) -> a -> Dict k v -> a
foldr3 op acc dict =
    foldrHelper2 op acc dict []


type Cont2 k v
    = Cont2 k v (Dict k v)


foldrHelper2 : (k -> v -> a -> a) -> a -> Dict k v -> List (Cont2 k v) -> a
foldrHelper2 op acc dict cont =
    case dict of
        Empty ->
            case cont of
                [] ->
                    acc

                (Cont2 key val next) :: tail ->
                    foldrHelper2 op (op key val acc) next tail

        Node _ key val left right ->
            foldrHelper2
                op
                acc
                right
                (Cont2 key val left :: cont)


foldr4 : (k -> v -> a -> a) -> a -> Dict k v -> a
foldr4 op acc dict =
    foldrHelper3 op acc dict []


foldrHelper3 : (k -> v -> a -> a) -> a -> Dict k v -> List ( k, v, Dict k v ) -> a
foldrHelper3 op acc dict cont =
    case dict of
        Empty ->
            case cont of
                [] ->
                    acc

                ( key, val, next ) :: tail ->
                    foldrHelper3 op (op key val acc) next tail

        Node _ key val left right ->
            foldrHelper3
                op
                acc
                right
                (( key, val, left ) :: cont)


{-| Keep a key-value pair when it satisfies a predicate.

    >>> fromList [ ( 1, "first" ), ( 2, "second" ) ]
    ... |> filter (\k v -> k > 1)
    ... |> toList
    [ ( 2, "second" ) ]
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

    >>> fromList [ (1, "first"), (2, "second") ]
    ... |> partition (\k v -> k < 2)
    ... |> Tuple.mapFirst toList
    ... |> Tuple.mapSecond toList
    ( [ ( 1, "first" ) ]
    , [ ( 2, "second" ) ]
    )
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

    >>> fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]
    ... |> keys
    [ 0
    , 1
    ]
-}
keys : Dict comparable v -> List comparable
keys dict =
    foldr (\key _ keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary, in the order of their keys.

    >>> values (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ])
    [ "Alice"
    , "Bob"
    ]
-}
values : Dict comparable v -> List v
values dict =
    foldr (always (::)) [] dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by
keys.

    >>> fromList [ (1, "Bob"), (0, "Alice") ]
    ... |> toList
    [ (0, "Alice")
    , (1, "Bob")
    ]
-}
toList : Dict comparable v -> List ( comparable, v )
toList dict =
    foldr (\key value list -> ( key, value ) :: list) [] dict


{-| Convert an association list into a dictionary.

    >>> fromList [ ( "key", "value" ) ]
    ... |> eq (singleton "key" "value")
    True
-}
fromList : List ( comparable, v ) -> Dict comparable v
fromList assocs =
    List.foldl
        (\( key, value ) dict -> update key (\_ -> Just value) dict)
        empty
        assocs


fromList2 : List ( comparable, v ) -> Dict comparable v
fromList2 list =
    --    List.sortWith reversePairs list
    List.sortBy Tuple.first list
        |> List.reverse
        |> uniqueReverseAndCount
        |> uncurry fromSortedList


reversePairs : ( comparable, a ) -> ( comparable, a ) -> Order
reversePairs ( a, _ ) ( b, _ ) =
    compare b a


uniqueReverseAndCount : List ( a, b ) -> ( Int, List ( a, b ) )
uniqueReverseAndCount list =
    case list of
        [] ->
            ( 0, [] )

        x :: xs ->
            let
                ( first, rest, count ) =
                    List.foldl
                        (\( ck, kv ) ( ( pk, pv ), res, count ) ->
                            if (ck == pk) then
                                ( ( pk, pv ), res, count )
                            else
                                ( ( ck, kv ), ( pk, pv ) :: res, count + 1 )
                        )
                        ( x, [], 1 )
                        xs
            in
                ( count, first :: rest )


fromSortedList : Int -> List ( comparable, v ) -> Dict comparable v
fromSortedList size list =
    if size == 0 then
        empty
    else
        let
            leftSize =
                size // 2

            left =
                List.take leftSize list

            rest =
                List.drop leftSize list
        in
            case rest of
                [] ->
                    empty

                ( k, v ) :: right ->
                    Node
                        ((logBase 2 (toFloat size) |> ceiling) + 1)
                        k
                        v
                        (fromSortedList leftSize left)
                        (fromSortedList (size - leftSize - 1) right)



-- Internal, implementation-specific operations


{-| Create a new set with a given element and a predecided lefthand and
righthand value. If both left and right are Empty, returns a Singleton.

Note that this constructor does *not* use insertion internally, and - hence -
does not ensure the result is balanced. This is a helper function that should
generally be followed by a balance; and this balancing act should bubble up the
entire structure.
-}
build : k -> v -> Dict k v -> Dict k v -> Dict k v
build key value left right =
    Node
        (max
            (height left)
            (height right)
            + 1
        )
        key
        value
        left
        right


{-| The height of a tree is something baked right into the Tree, and is
important for balancing the internal tree. It is defined as the maximal height
of its branches + 1. A properly balanced tree will have a maximal
height-difference between branches of |1|.
-}
height : Dict k v -> Int
height dict =
    case dict of
        Empty ->
            0

        Node nodeHeight _ _ _ _ ->
            nodeHeight


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
            case heightDiff dict of
                (-2) ->
                    if heightDiff left == 1 then
                        {- left leaning tree with right-leaning left
                           subtree. Rotate the left subtree to the left,
                           then rotate the entire tree to the right.
                        -}
                        rotateRight
                            (Node level key value (rotateLeft left) right)
                    else
                        -- left leaning tree, generally. Rotate right.
                        rotateRight dict

                2 ->
                    if heightDiff right == -1 then
                        {- right leaning tree with left-leaning right
                           subtree. Rotate right subtree to the right, then
                           rotate the entire tree to the left.
                        -}
                        rotateLeft
                            (Node level key value left (rotateRight right))
                    else
                        -- right leaning tree, generally. Rotate left.
                        rotateLeft dict

                _ ->
                    -- diff is -1, 0, or 1. Already balanced!
                    dict


{-| Convenience function to check a given predicate against all entries.
-}
all : (comparable -> v -> Bool) -> Dict comparable v -> Bool
all predicate dict =
    foldl (\k v c -> c && predicate k v) True dict == True


{-| Checks whether a given Dict is a valid binary search tree internally.
Should always be true.

    >>> isValidBst <| fromList [ ( 1, () ), ( 2, () ), ( 0, () ) ]
    True

    >>> isValidBst empty
    True
-}
isValidBst : Dict comparable v -> Bool
isValidBst dict =
    case dict of
        Empty ->
            True

        Node _ key _ left right ->
            all (\k _ -> k < key) left
                && all (\k _ -> k > key) right
                && isValidBst left
                && isValidBst right


{-| Checks whether a given Dict is a valid AVL tree internally. Should always
be true.

    >>> isValidAvl <| fromList [ ( 1, () ), ( 2, () ), ( 0, () ) ]
    True

    >>> isValidAvl empty
    True
-}
isValidAvl : Dict comparable v -> Bool
isValidAvl tree =
    case tree of
        Empty ->
            True

        Node _ _ _ left right ->
            (heightDiff tree |> abs)
                <= 1
                && isValidAvl left
                && isValidAvl right
