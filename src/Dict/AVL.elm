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

import Tree.AVL as Tree exposing (Tree)


{-| A dictionary of keys and values. So a `(Dict String User)` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.
-}
type Dict k v
    = Dict (Tree k v)


{-| Create an empty dictionary.
-}
empty : Dict k v
empty =
    Dict Tree.empty


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : comparable -> Dict comparable v -> Maybe v
get key (Dict tree) =
    Tree.get key tree


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
size (Dict tree) =
    Tree.foldl (always <| always <| (+) 1) 0 tree


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
insert key value (Dict tree) =
    Dict <| Tree.update key (always <| Just value) tree


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : comparable -> Dict comparable v -> Dict comparable v
remove key (Dict tree) =
    Dict <| Tree.update key (always Nothing) tree


{-| Update the value of a dictionary for a specific key with a given function.
-}
update :
    comparable
    -> (Maybe v -> Maybe v)
    -> Dict comparable v
    -> Dict comparable v
update k alter (Dict tree) =
    Dict <| Tree.update k alter tree


{-| Create a dictionary with one key-value pair.
-}
singleton : comparable -> v -> Dict comparable v
singleton key value =
    Dict <| Tree.singleton key value



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
foldl : (comparable -> v -> b -> b) -> b -> Dict comparable v -> b
foldl op acc (Dict tree) =
    Tree.foldl op acc tree


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key.
-}
foldr : (comparable -> v -> b -> b) -> b -> Dict comparable v -> b
foldr op acc (Dict tree) =
    Tree.foldr op acc tree


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
    Dict <| Tree.fromList assocs
