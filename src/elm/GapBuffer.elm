module GapBuffer exposing
    ( Buffer
    , empty, fromArray, fromList
    , get, isEmpty, length, slice
    , getFocus, setFocus, insertAtFocus, updateFocus
    , foldlSlice, foldrSlice
    )

{-| Implements an efficient Buffer for text editing.


# Make a Buffer

@docs Buffer
@docs empty, fromArray, fromList


# Query

@docs get, isEmpty, length, slice


# Manipulate

@docs getFocus, setFocus, insertAtFocus, updateFocus


# Iterate

@docs foldlSlice, foldrSlice

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
    if idx < 0 || idx >= buffer.length then
        buffer

    else
        case buffer.zip of
            Nothing ->
                rezip idx buffer

            Just zip ->
                if zip.at == idx then
                    buffer

                else
                    rezip idx buffer


rezip : Int -> Buffer a b -> Buffer a b
rezip idx buffer =
    { buffer
        | head = slice 0 idx buffer
        , zip =
            get idx buffer
                |> Maybe.map
                    (\val ->
                        { val = buffer.toZip val
                        , at = idx
                        , tail = slice (idx + 1) buffer.length buffer
                        }
                    )
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
                Array.get (idx - zip.at - 1) zip.tail


{-| Extracts a slice of data from the buffer, between the _from_ and _to_ indices
specified.

If these indicies go outside the range of the `Buffer`, data from the
actual available range will be returned.

If you are iterating over the contents of the buffer, to render a
UI for example, there is no need to copy the contents into an intermediate
`Array`. You can iterate directly over a region of the buffer using the
`foldlSlice` function instead.

-}
slice : Int -> Int -> Buffer a b -> Array a
slice from to buffer =
    let
        intersects s1 e1 s2 e2 =
            s1 < e2 && e1 >= s2
    in
    case buffer.zip of
        Nothing ->
            Array.slice from to buffer.head

        Just zip ->
            let
                headLength =
                    Array.length buffer.head

                tailLength =
                    Array.length zip.tail

                tailStart =
                    zip.at + 1

                s1 =
                    if intersects from to 0 headLength then
                        Array.slice
                            (max 0 from)
                            (min headLength to)
                            buffer.head

                    else
                        Array.empty

                s2 =
                    if zip.at >= from && zip.at <= to then
                        Array.push (buffer.toArray zip.val) s1

                    else
                        s1

                s3 =
                    if intersects from to tailStart (tailLength + tailStart) then
                        Array.append
                            s2
                            (Array.slice
                                (max 0 (from - tailStart))
                                (min tailLength (to - tailStart))
                                zip.tail
                            )

                    else
                        s2
            in
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


insertAtFocus : Int -> b -> Buffer a b -> Buffer a b
insertAtFocus idx val buffer =
    if idx < 0 || idx > buffer.length then
        buffer

    else
        { buffer
            | head = slice 0 idx buffer
            , zip =
                { val = val
                , at = idx
                , tail = slice idx buffer.length buffer
                }
                    |> Just
            , length = buffer.length + 1
        }


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


{-| Update the value at the specified focus of the `Buffer`. If the `Buffer` was
already focussed at a different index, that index will be de-focussed, and the
focus shifted to the specified index.

Note that de-focussing and re-focussing the `Buffer` will use the `toZip` and
`toArray` functions that were specified when creating the buffer.

-}
updateFocus : Int -> (b -> b) -> Buffer a b -> Buffer a b
updateFocus idx fn buffer =
    let
        rezipped =
            zipAt idx buffer
    in
    { rezipped | zip = rezipped.zip |> Maybe.map (\zip -> { zip | val = fn zip.val }) }



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


{-| Iterates backward over a region of the `Buffer`.

This is the most efficient way to extract and map data from the buffer. For
example, you would use this when rendering the visible contents of a `Buffer`
to Html. The implementation does not create intermediate data structures to hold
the extracted elements, and it only iterates over the range you specify.

-}
foldrSlice : (Int -> a -> acc -> acc) -> acc -> Int -> Int -> Buffer a b -> acc
foldrSlice fn acc from to buffer =
    List.foldr
        (\idx resAcc ->
            case get idx buffer of
                Just val ->
                    fn idx val resAcc

                Nothing ->
                    resAcc
        )
        acc
        (List.range from to)
