module Buffer exposing
    ( empty, fromArray, fromList
    , get, isEmpty, length, slice
    , getFocus, setFocus
    , foldlSlice
    )

{-| Implements an efficient Buffer for text editing.


# Make a Buffer

@docs empty, fromArray, fromList


# Query

@docs get, isEmpty, length, slice


# Manipulate

@docs getFocus, setFocus


# Iterate

@docs foldlSlice

-}

import Array exposing (Array)


type alias Buffer a b =
    { head : Array a
    , zip :
        Maybe
            { val : b
            , at : Int
            , tail : Array a
            }
    , length : Int
    , toZip : a -> b
    , toArray : b -> a
    }


zipAt : Int -> Buffer a b -> Buffer a b
zipAt =
    Debug.todo "zipAt"



-- Make a Buffer


{-| Creates an empty `Buffer`.
-}
empty : (a -> b) -> (b -> a) -> Buffer a b
empty toZip toArray =
    { head = Array.empty
    , zip = Nothing
    , length = 0
    , toZip = toZip
    , toArray = toArray
    }


{-| Creates a `Buffer` from a `List`.
-}
fromList : (a -> b) -> (b -> a) -> List a -> Buffer a b
fromList toZip toArray list =
    let
        array =
            Array.fromList list
    in
    { head = array
    , zip = Nothing
    , length = Array.length array
    , toZip = toZip
    , toArray = toArray
    }


{-| Creates a `Buffer` from an `Array`.
-}
fromArray : (a -> b) -> (b -> a) -> Array a -> Buffer a b
fromArray toZip toArray array =
    { head = array
    , zip = Nothing
    , length = Array.length array
    , toZip = toZip
    , toArray = toArray
    }



-- Query


{-| Checks if a `Buffer` is empty.
-}
isEmpty : Buffer a b -> Bool
isEmpty buffer =
    buffer.length == 0


{-| Gets the number of element in the `Buffer`.
-}
length : Buffer a b -> Int
length buffer =
    buffer.length


{-| Extracts the element at the specified index in the `Buffer`.
If the `Buffer` does not hold data for this index, `Nothing` is returned.
-}
get : Int -> Buffer a b -> Maybe a
get idx buffer =
    Debug.todo "get"


{-| Extracts a slice of data from the buffer, between the _from_ and _to_ indices
specified.

If these indicies go outside the range of the `Buffer`, data from the
actual available range will be returned.

-}
slice : Int -> Int -> Buffer a b -> Array a
slice from to buffer =
    Debug.todo "slice"



-- Manipulate


{-| Sets the value as the focus of the `Buffer`.
If the `Buffer` was already focussed at a different index, that index will be
de-focussed, and the focus shifted to the specified index.

Note that de-focussing and re-focussing the `Buffer` will use the `toZip` and
`toArray` functions that were specified when creating the buffer.

-}
setFocus : Int -> b -> Buffer a b -> Buffer a b
setFocus =
    Debug.todo "set"


{-| Gets the value at the specified focus of the `Buffer`. If the `Buffer` was
already focussed at a different index, that index will be de-focussed, and the
focus shifted to the specified index.

Note that de-focussing and re-focussing the `Buffer` will use the `toZip` and
`toArray` functions that were specified when creating the buffer.

-}
getFocus : Int -> Buffer a b -> b
getFocus =
    Debug.todo "getFocus"



-- Iterate


{-| Iterates forward over a region of the `Buffer`.

This is the most efficient way to extract and map data from the buffer. For
example, you would use this when rendering the visible contents of a `Buffer`
to Html. The implementation does not create intermeidate data structures to hold
the extracted elements, and it only iterates over the range you specify.

-}
foldlSlice : (Int -> a -> acc -> acc) -> acc -> Int -> Int -> Buffer a b -> acc
foldlSlice =
    Debug.todo "foldlSlice"
