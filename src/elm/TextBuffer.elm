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
import GapBuffer exposing (Buffer)
import Regex



-- Buffers and ways to make one.


type alias TextBuffer =
    Buffer String (Buffer Char Char)


stringToCharBuffer : String -> Buffer Char Char
stringToCharBuffer string =
    String.toList string |> GapBuffer.fromList identity identity


charBufferToString : Buffer Char Char -> String
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
--
--
-- breakLine : Int -> Int -> TextBuffer -> TextBuffer
-- breakLine line column buffer =
--     let
--         line_ =
--             line + 1
--
--         linesList =
--             allLines buffer
--
--         contentUntilCursor =
--             linesList
--                 |> List.take line_
--                 |> List.indexedMap
--                     (\i content ->
--                         if i == line then
--                             String.left column content
--
--                         else
--                             content
--                     )
--
--         restOfLineAfterCursor =
--             String.dropLeft column (getLine line buffer)
--
--         restOfLines =
--             List.drop line_ linesList
--     in
--     (contentUntilCursor
--         ++ [ restOfLineAfterCursor ]
--         ++ restOfLines
--     )
--         |> fromList


insertCharAt : Char -> Int -> Int -> TextBuffer -> TextBuffer
insertCharAt char row col buffer =
    GapBuffer.updateFocus row
        (\rowBuffer -> GapBuffer.updateFocus col (always char) rowBuffer)
        buffer



-- deleteCharBefore : Int -> Int -> TextBuffer -> TextBuffer
-- deleteCharBefore line column buffer =
--     let
--         removeCharFromLine ( lineNum, content ) =
--             if lineNum == line - 1 then
--                 if isFirstColumn column then
--                     [ content ++ getLine line buffer ]
--
--                 else
--                     [ content ]
--
--             else if lineNum == line then
--                 if isFirstColumn column then
--                     []
--
--                 else
--                     [ String.left (column - 1) content
--                         ++ String.dropLeft column content
--                     ]
--
--             else
--                 [ content ]
--     in
--     buffer
--         |> toIndexedList
--         |> List.concatMap removeCharFromLine
--         |> fromList
--
--
-- deleteCharAfter : Int -> Int -> TextBuffer -> TextBuffer
-- deleteCharAfter line column buffer =
--     let
--         isOnLastColumn =
--             isLastColumn buffer line column
--
--         removeCharFromLine ( lineNum, content ) =
--             if lineNum == line then
--                 if isOnLastColumn then
--                     [ content ++ getLine  (line + 1) buffer ]
--
--                 else
--                     [ String.left column content
--                         ++ String.dropLeft (column + 1) content
--                     ]
--
--             else if lineNum == line + 1 then
--                 if isOnLastColumn then
--                     []
--
--                 else
--                     [ content ]
--
--             else
--                 [ content ]
--     in
--     buffer
--         |> toIndexedList
--         |> List.concatMap removeCharFromLine
--         |> fromList
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
