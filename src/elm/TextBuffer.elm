module TextBuffer exposing (..)

{-| An editing buffer.


# Buffers and ways to make one.

@docs TextBuffer, fromString, fromList


# Make changes to a buffers contents.

@docs breakLine, deleteCharAfter, deleteCharBefore, insertCharAt


# Get the contents from a buffer.

@docs range, charAt, contents


# Iterate over the buffer contents.

@docs indexedFoldl


# Query size and position information.

@docs isFirstLine, isFirstColumn, isLastLine, isLastColumn
@docs lastColumn, lastLine, nextLine, previousLine, numLines, clampColumn

-}

-- ( TextBuffer, fromString, fromList
-- , breakLine, deleteCharAfter, deleteCharBefore, insertCharAt
-- , range, charAt, contents
-- , indexedFoldl
-- , isFirstLine, isFirstColumn, isLastLine, isLastColumn
-- , lastColumn, lastLine, nextLine, previousLine, numLines, clampColumn
-- )

import Array exposing (Array)
import GapBuffer exposing (GapBuffer)
import Regex



-- Buffers and ways to make one.


type alias TextBuffer =
    GapBuffer String (GapBuffer Char Char)


stringToCharBuffer : String -> GapBuffer Char Char
stringToCharBuffer string =
    String.toList string |> GapBuffer.fromList identity identity


charBufferToString : GapBuffer Char Char -> String
charBufferToString charBuffer =
    GapBuffer.foldrSlice
        (\_ char accum -> char :: accum)
        []
        0
        (GapBuffer.length charBuffer)
        charBuffer
        |> String.fromList


empty : TextBuffer
empty =
    GapBuffer.empty stringToCharBuffer charBufferToString


fromString : String -> TextBuffer
fromString source =
    newlineRegex
        |> (\r -> Regex.split r source)
        |> GapBuffer.fromList stringToCharBuffer charBufferToString


fromList : List String -> TextBuffer
fromList lines =
    GapBuffer.fromList stringToCharBuffer charBufferToString lines


fromArray : Array String -> TextBuffer
fromArray array =
    GapBuffer.fromArray stringToCharBuffer charBufferToString array



-- Shift the buffer focus


{-| Shift the buffer focues without changing the contents.
-}
refocus row col buffer =
    GapBuffer.updateFocus row
        (\rowBuffer -> GapBuffer.updateFocus col identity rowBuffer)
        buffer



-- Make changes to a buffers contents


breakLine : Int -> Int -> TextBuffer -> TextBuffer
breakLine row col buffer =
    case GapBuffer.getFocus row buffer of
        ( _, Nothing ) ->
            buffer

        ( focussedBuffer, Just rowBuffer ) ->
            let
                lineBeforeCursor =
                    GapBuffer.slice 0 col rowBuffer
                        |> GapBuffer.fromArray rowBuffer.toZip rowBuffer.toArray

                lineAfterCursor =
                    GapBuffer.slice col rowBuffer.length rowBuffer
                        |> GapBuffer.fromArray rowBuffer.toZip rowBuffer.toArray
            in
            focussedBuffer
                |> GapBuffer.setFocus row lineBeforeCursor
                |> GapBuffer.insertAtFocus (row + 1) lineAfterCursor


insertCharAt : Char -> Int -> Int -> TextBuffer -> TextBuffer
insertCharAt char row col buffer =
    GapBuffer.updateFocus row
        (\rowBuffer -> GapBuffer.insertAtFocus col char rowBuffer)
        buffer


deleteCharBefore : Int -> Int -> TextBuffer -> TextBuffer
deleteCharBefore row col buffer =
    if isFirstColumn col && isFirstLine row then
        buffer

    else if isFirstColumn col then
        case GapBuffer.getFocus row buffer of
            ( _, Nothing ) ->
                buffer

            ( focussedBuffer, Just rowBuffer ) ->
                focussedBuffer
                    |> GapBuffer.delete row
                    |> GapBuffer.updateFocus (row - 1)
                        (\prevRowBuffer ->
                            Array.append
                                (GapBuffer.slice 0 prevRowBuffer.length prevRowBuffer)
                                (GapBuffer.slice 0 rowBuffer.length rowBuffer)
                                |> GapBuffer.fromArray rowBuffer.toZip rowBuffer.toArray
                        )

    else
        GapBuffer.updateFocus row
            (\rowBuffer -> GapBuffer.delete (col - 1) rowBuffer)
            buffer


deleteCharAt : Int -> Int -> TextBuffer -> TextBuffer
deleteCharAt row col buffer =
    if isLastColumn buffer row col && isLastLine buffer row then
        buffer

    else if isLastColumn buffer row col then
        case GapBuffer.getFocus (row + 1) buffer of
            ( _, Nothing ) ->
                buffer

            ( focussedBuffer, Just nextRowBuffer ) ->
                focussedBuffer
                    |> GapBuffer.delete (row + 1)
                    |> GapBuffer.updateFocus row
                        (\rowBuffer ->
                            Array.append
                                (GapBuffer.slice 0 rowBuffer.length rowBuffer)
                                (GapBuffer.slice 0 nextRowBuffer.length nextRowBuffer)
                                |> GapBuffer.fromArray rowBuffer.toZip rowBuffer.toArray
                        )

    else
        GapBuffer.updateFocus row
            (\rowBuffer -> GapBuffer.delete col rowBuffer)
            buffer



--
--
--
-- Get the contents from a buffer.
--
--
-- range : Int -> Int -> Int -> Int -> TextBuffer -> String
-- range r1 c1 r2 c2 buffer =
--     let
--         numberOfLines =
--             r2 - r1 + 1
--     in
--     buffer
--         |> toList
--         |> List.drop r1
--         |> List.take numberOfLines
--         |> List.indexedMap
--             (\i line ->
--                 if numberOfLines == 1 then
--                     line
--                         |> String.dropLeft c1
--                         |> String.left (c2 - c1 + 1)
--
--                 else if i == 0 then
--                     String.dropLeft c1 line
--
--                 else if i == numberOfLines - 1 then
--                     String.left (c2 + 1) line
--
--                 else
--                     line
--             )
--         |> String.join "\n"
--
--


charAt : Int -> Int -> TextBuffer -> String
charAt line column buffer =
    getLine line buffer
        |> Maybe.withDefault ""
        |> String.dropLeft column
        |> String.left 1


contents : TextBuffer -> String
contents buffer =
    foldlLines
        (\line ( isFirst, accum ) ->
            ( False
            , if isFirst then
                accum ++ line

              else
                accum ++ "\n" ++ line
            )
        )
        ( True, "" )
        buffer
        |> Tuple.second



-- Iterate over the buffer contents.


foldlSlice : (Int -> String -> acc -> acc) -> acc -> Int -> Int -> TextBuffer -> acc
foldlSlice fn accum buffer =
    GapBuffer.foldlSlice fn accum buffer



-- Query size and position information.


isFirstLine : Int -> Bool
isFirstLine line =
    line == 0


isLastLine : TextBuffer -> Int -> Bool
isLastLine buffer line =
    line == lastLine buffer


isFirstColumn : Int -> Bool
isFirstColumn column =
    column == 0


isLastColumn : TextBuffer -> Int -> Int -> Bool
isLastColumn buffer line column =
    column == lastColumn buffer line


lastLine : TextBuffer -> Int
lastLine buffer =
    GapBuffer.length buffer - 1


lastColumn : TextBuffer -> Int -> Int
lastColumn buffer line =
    lineLength line buffer


previousLine : Int -> Int
previousLine line =
    (line - 1)
        |> max 0


nextLine : TextBuffer -> Int -> Int
nextLine buffer line =
    (line + 1)
        |> min (lastLine buffer)


length : TextBuffer -> Int
length buffer =
    GapBuffer.length buffer


clampColumn : TextBuffer -> Int -> Int -> Int
clampColumn buffer line column =
    column
        |> clamp 0 (lineLength line buffer)



-- Helpers
-- toIndexedList : TextBuffer -> List ( Int, String )
-- toIndexedList buffer =
--     Array.toIndexedList buffer
--
--
-- get : Int -> TextBuffer -> Maybe String
-- get line buffer =
--     Array.get line buffer
--
--
-- toList : TextBuffer -> List String
-- toList buffer =
--     Array.toList buffer
--
--


getLine : Int -> TextBuffer -> Maybe String
getLine lineNum buffer =
    GapBuffer.get lineNum buffer


lineLength : Int -> TextBuffer -> Int
lineLength lineNum buffer =
    getLine lineNum buffer
        |> Maybe.withDefault ""
        |> String.length



--
--
-- allLines : TextBuffer -> List String
-- allLines buffer =
--     Array.toList buffer
--
--


foldlLines : (String -> a -> a) -> a -> TextBuffer -> a
foldlLines fn init buffer =
    GapBuffer.foldlSlice
        (\_ line acc -> fn line acc)
        init
        0
        (GapBuffer.length buffer)
        buffer



--
--
-- indexedMap : (Int -> String -> String) -> TextBuffer -> TextBuffer
-- indexedMap fn buffer =
--     Array.indexedMap fn buffer
--
--


newlineRegex =
    Regex.fromString "\\n"
        |> Maybe.withDefault Regex.never
