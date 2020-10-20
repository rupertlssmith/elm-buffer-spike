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
zipAt idx buffer =
    let
        dezipped =
            dezip buffer
    in
    if idx < buffer.length then
        dezipped

    else
        { dezipped
            | head = Array.slice 0 (idx - 1) buffer.head
            , zip =
                Array.get idx buffer.head
                    |> Maybe.map
                        (\val ->
                            { val = buffer.toZip val
                            , at = idx
                            , tail = Array.slice (idx + 1) buffer.length buffer.head
                            }
                        )
        }


dezip : Buffer a b -> Buffer a b
dezip buffer =
    case buffer.zip of
        Nothing ->
            buffer

        Just zip ->
            { buffer
                | head =
                    Array.append
                        buffer.head
                        (Array.append
                            (Array.fromList [ buffer.toArray zip.val ])
                            zip.tail
                        )
                , zip = Nothing
            }



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
    case buffer.zip of
        Nothing ->
            Array.get idx buffer.head

        Just zip ->
            if idx < Array.length buffer.head then
                Array.get idx buffer.head

            else if idx == zip.at then
                buffer.toArray zip.val |> Just

            else
                Array.get (idx - zip.at) zip.tail


{-| Extracts a slice of data from the buffer, between the _from_ and _to_ indices
specified.

If these indicies go outside the range of the `Buffer`, data from the
actual available range will be returned.

-}
slice : Int -> Int -> Buffer a b -> Array a
slice from to buffer =
    let
        intersects _ _ _ _ =
            True
    in
    case buffer.zip of
        Nothing ->
            Array.slice from to buffer.head

        Just zip ->
            let
                s1 =
                    if intersects from to 0 buffer.head then
                        Array.slice from to buffer.head :: []

                    else
                        []

                s2 =
                    if zip.at >= from && zip.at <= to then
                        Array.push (buffer.toArray zip.val) Array.empty :: s1

                    else
                        s1

                s3 =
                    if intersects from to zip.at zip.tail then
                        Array.slice (from - zip.at) (to - zip.at) zip.tail :: s2

                    else
                        s2
            in
            List.foldl
                (\next accum -> Array.append next accum)
                Array.empty
                s3



-- Manipulate


{-| Sets the value as the focus of the `Buffer`.
If the `Buffer` was already focussed at a different index, that index will be
de-focussed, and the focus shifted to the specified index.

Note that de-focussing and re-focussing the `Buffer` will use the `toZip` and
`toArray` functions that were specified when creating the buffer.

-}
setFocus : Int -> b -> Buffer a b -> Buffer a b
setFocus idx val buffer =
    let
        rezipped =
            zipAt idx buffer
    in
    { rezipped | zip = rezipped.zip |> Maybe.map (\zip -> { zip | val = val }) }


{-| Gets the value at the specified focus of the `Buffer`. If the `Buffer` was
already focussed at a different index, that index will be de-focussed, and the
focus shifted to the specified index.

Note that de-focussing and re-focussing the `Buffer` will use the `toZip` and
`toArray` functions that were specified when creating the buffer.

-}
getFocus : Int -> Buffer a b -> ( Buffer a b, Maybe b )
getFocus idx buffer =
    let
        rezipped =
            zipAt idx buffer
    in
    ( rezipped, rezipped.zip |> Maybe.map .val )



-- Iterate


{-| Iterates forward over a region of the `Buffer`.

This is the most efficient way to extract and map data from the buffer. For
example, you would use this when rendering the visible contents of a `Buffer`
to Html. The implementation does not create intermediate data structures to hold
the extracted elements, and it only iterates over the range you specify.

-}
foldlSlice : (Int -> a -> acc -> acc) -> acc -> Int -> Int -> Buffer a b -> acc
foldlSlice fn acc from to buffer =
    List.foldl
        (\idx resAcc ->
            case get idx buffer of
                Just val ->
                    fn idx val resAcc

                Nothing ->
                    resAcc
        )
        acc
        (List.range from to)
