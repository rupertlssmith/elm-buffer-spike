module Buffer exposing
    ( empty, fromArray, fromList
    , get, isEmpty, length, slice
    , getFocus, set
    , foldlSlice
    )

{-| Implements an efficient Buffer for text editing.


# Make a Buffer

@docs empty, fromArray, fromList


# Query

@docs get, isEmpty, length, slice


# Manipulate

@docs getFocus, set


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


empty : (a -> b) -> (b -> a) -> Buffer a b
empty toZip toArray =
    { head = Array.empty
    , zip = Nothing
    , length = 0
    , toZip = toZip
    , toArray = toArray
    }


fromList : (a -> b) -> (b -> a) -> List a -> Buffer a b
fromList =
    Debug.todo "fromList"


fromArray : (a -> b) -> (b -> a) -> Array a -> Buffer a b
fromArray =
    Debug.todo "fromArray"



-- Query


isEmpty : Buffer a b -> Bool
isEmpty =
    Debug.todo "isEmpty"


length : Buffer a b -> Int
length =
    Debug.todo "length"


get : Int -> Buffer a b -> a
get =
    Debug.todo "get"


slice : Int -> Int -> Buffer a b -> Array a
slice =
    Debug.todo "slice"



-- Manipulate


set : Int -> b -> Buffer a b -> Buffer a b
set =
    Debug.todo "set"


getFocus : Int -> Buffer a b -> b
getFocus =
    Debug.todo "getFocus"



-- Iterate


foldlSlice : (Int -> a -> acc -> acc) -> acc -> Int -> Int -> Buffer a b -> acc
foldlSlice =
    Debug.todo "foldlSlice"
