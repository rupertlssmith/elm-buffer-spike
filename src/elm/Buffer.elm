module Buffer exposing (..)

import Array exposing (Array)


type alias ZippedArray a b =
    { head : Array a
    , zip : b
    , zipAt : Int
    , tail : Array a
    , length : Int
    , toZip : a -> b
    , toArray : b -> a
    }


type alias UnzippedArray a b =
    { array : Array a
    , toZip : a -> b
    , toArray : b -> a
    }


type Buffer a b
    = Zipped (ZippedArray a b)
    | Unzipped (UnzippedArray a b)


dezip : ZipArray a b -> Array a
dezip =
    Debug.log "dezip"


zip : Array a -> ZipArray a b
zip =
    Debug.log "zip"


rezip : ZipArray a b -> ZipArray a b
rezip =
    Debug.log "rezip"


empty : (a -> b) -> (b -> a) -> Buffer a b
empty toZip toArray =
    Unzipped
        { array = Array.empty
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
