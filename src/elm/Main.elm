module Main exposing (main)

--import GapBuffer exposing (Buffer)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Dom exposing (Viewport)
import Browser.Events
import Css
import Css.Global
import GapBuffer
import Html as H exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Keyed as Keyed
import Html.Lazy
import Html.Styled
import Json.Decode as Decode exposing (Decoder)
import Random exposing (Generator, Seed)
import Random.Array
import Regex exposing (Regex)
import Task exposing (Task)
import TextBuffer exposing (TextBuffer)
import Time exposing (Posix)


config =
    let
        fontSize =
            15

        lineHeightRatio =
            1.4
    in
    { fontSize = fontSize
    , lineHeightRatio = lineHeightRatio
    , lineHeight = (lineHeightRatio * fontSize) |> floor |> toFloat
    , lineLength = 120
    , numLines = 10000
    , blinkInterval = 400
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { buffer : TextBuffer Tag Tag
    , top : Float
    , height : Float
    , cursor : RowCol
    , scrollRow : Int
    , targetCol : Int
    , linesPerPage : Int
    , bottomOffset : Float
    , blinker : Bool
    , lastActive : Posix
    }


type alias RowCol =
    { row : Int
    , col : Int
    }


init _ =
    ( { buffer = TextBuffer.empty initialCtx tagLineFn
      , top = 0
      , height = 0
      , cursor = { row = 0, col = 0 }
      , scrollRow = 0
      , targetCol = 0
      , linesPerPage = 0
      , bottomOffset = 0.0
      , blinker = False
      , lastActive = Time.millisToPosix 0
      }
    , Cmd.batch
        [ Task.perform RandomBuffer (randomBuffer config.lineLength config.numLines |> randomToTask)
        , initEditorSize
        , Browser.Dom.focus "editor-main" |> Task.attempt (always NoOp)
        ]
    )



-- Buffer setup.


type Tag
    = NormalText
    | QuotedText


initialCtx : Tag
initialCtx =
    NormalText


tagLineFn : TextBuffer.TagLineFn Tag Tag
tagLineFn charBuffer startCtx =
    let
        pushTag ( tagAccum, lineAccum, ctx ) =
            ( ( ctx, tagAccum |> List.reverse |> String.fromList ) :: lineAccum |> List.reverse, ctx )

        ( tagged, endCtx ) =
            GapBuffer.indexedFoldl
                (\_ char ( tagAccum, lineAccum, ctx ) ->
                    case ( char, ctx ) of
                        ( '"', NormalText ) ->
                            ( [ char ], ( ctx, tagAccum |> List.reverse |> String.fromList ) :: lineAccum, QuotedText )

                        ( '"', QuotedText ) ->
                            ( [], ( ctx, char :: tagAccum |> List.reverse |> String.fromList ) :: lineAccum, NormalText )

                        ( _, _ ) ->
                            ( char :: tagAccum, lineAccum, ctx )
                )
                ( [], [], startCtx )
                charBuffer
                |> pushTag
    in
    ( tagged, endCtx )



-- Events and event handling.


subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize (\_ _ -> Resize)
        , Time.every config.blinkInterval Blink
        ]


type Msg
    = Scroll ScrollEvent
    | RandomBuffer (TextBuffer Tag Tag)
    | ContentViewPort (Result Browser.Dom.Error Viewport)
    | Resize
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | PageUp
    | PageDown
    | LineHome
    | LineEnd
    | FileHome
    | FileEnd
    | InsertChar Char
    | RemoveCharBefore
    | RemoveCharAfter
    | NewLine
    | Blink Posix
    | Activity Posix
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomBuffer buffer ->
            ( { model | buffer = buffer }, Cmd.none )

        Scroll scroll ->
            ( { model
                | top = scroll.scrollTop
                , scrollRow = scroll.scrollTop / config.lineHeight |> round
              }
            , Cmd.none
            )

        ContentViewPort result ->
            case result of
                Ok viewport ->
                    ( { model
                        | height = viewport.viewport.height
                        , bottomOffset = bottomOffset viewport.viewport.height
                        , linesPerPage = linesPerPage viewport.viewport.height
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Resize ->
            ( model, initEditorSize )

        MoveUp ->
            ( model, Cmd.none )
                |> andThen (moveCursorRowBy -1)
                --|> andThen refocusBuffer
                |> andThen scrollIfNecessary
                |> andThen activity

        MoveDown ->
            ( model, Cmd.none )
                |> andThen (moveCursorRowBy 1)
                --|> andThen refocusBuffer
                |> andThen scrollIfNecessary
                |> andThen rippleBuffer
                |> andThen activity

        MoveLeft ->
            let
                lastColPrevRow =
                    TextBuffer.lastColumn model.buffer (model.cursor.row - 1)
            in
            ( model, Cmd.none )
                |> andThen (cursorLeft lastColPrevRow)
                --|> andThen refocusBuffer
                |> andThen scrollIfNecessary
                |> andThen activity

        MoveRight ->
            ( model, Cmd.none )
                |> andThen cursorRight
                --|> andThen refocusBuffer
                |> andThen scrollIfNecessary
                |> andThen rippleBuffer
                |> andThen activity

        PageUp ->
            ( model, Cmd.none )
                |> andThen (moveCursorRowBy -model.linesPerPage)
                --|> andThen refocusBuffer
                |> andThen scrollIfNecessary
                |> andThen activity

        PageDown ->
            ( model, Cmd.none )
                |> andThen (moveCursorRowBy model.linesPerPage)
                --|> andThen refocusBuffer
                |> andThen scrollIfNecessary
                |> andThen rippleBuffer
                |> andThen activity

        LineHome ->
            ( model, Cmd.none )
                |> andThen (moveCursorColBy -model.cursor.col)
                |> andThen scrollIfNecessary
                |> andThen activity

        LineEnd ->
            ( model, Cmd.none )
                |> andThen (moveCursorColBy (TextBuffer.lastColumn model.buffer model.cursor.row - model.cursor.col))
                |> andThen scrollIfNecessary
                |> andThen activity

        FileHome ->
            ( model, Cmd.none )
                |> andThen (moveTo { row = 0, col = 0 })
                |> andThen scrollIfNecessary
                |> andThen activity

        FileEnd ->
            let
                lastRow =
                    TextBuffer.length model.buffer - 1
            in
            ( model, Cmd.none )
                |> andThen
                    (moveTo
                        { row = lastRow
                        , col = TextBuffer.lastColumn model.buffer lastRow
                        }
                    )
                |> andThen scrollIfNecessary
                |> andThen rippleBuffer
                |> andThen activity

        InsertChar char ->
            ( model, Cmd.none )
                |> andThen (insertChar char)
                |> andThen (moveCursorColBy 1)
                |> andThen rippleBuffer
                |> andThen activity

        RemoveCharBefore ->
            let
                lastColPrevRow =
                    TextBuffer.lastColumn model.buffer (model.cursor.row - 1)
            in
            ( model, Cmd.none )
                |> andThen backspace
                |> andThen (cursorLeft lastColPrevRow)
                |> andThen scrollIfNecessary
                |> andThen rippleBuffer
                |> andThen activity

        RemoveCharAfter ->
            ( model, Cmd.none )
                |> andThen delete
                |> andThen scrollIfNecessary
                |> andThen rippleBuffer
                |> andThen activity

        NewLine ->
            ( model, Cmd.none )
                |> andThen newline
                |> andThen (moveCursorRowBy 1)
                |> andThen (moveCursorColBy -model.cursor.col)
                |> andThen scrollIfNecessary
                |> andThen rippleBuffer
                |> andThen activity

        Blink posix ->
            if Time.posixToMillis posix - Time.posixToMillis model.lastActive > config.blinkInterval then
                ( { model | blinker = not model.blinker }, Cmd.none )

            else
                ( { model | blinker = True }, Cmd.none )

        Activity posix ->
            ( { model | lastActive = posix, blinker = True }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


andThen : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThen fn ( model, cmd ) =
    let
        ( nextModel, nextCmd ) =
            fn model
    in
    ( nextModel, Cmd.batch [ cmd, nextCmd ] )


moveTo : RowCol -> Model -> ( Model, Cmd Msg )
moveTo pos model =
    ( { model
        | cursor = pos
        , targetCol = pos.col
      }
    , Cmd.none
    )


moveCursorRowBy : Int -> Model -> ( Model, Cmd Msg )
moveCursorRowBy val model =
    let
        newRow =
            clamp
                0
                (TextBuffer.lastLine model.buffer)
                (model.cursor.row + val)

        newCol =
            clamp 0
                (TextBuffer.lastColumn model.buffer newRow)
                (max model.cursor.col model.targetCol)
    in
    ( { model | cursor = { row = newRow, col = newCol } }
    , Cmd.none
    )


moveCursorColBy : Int -> Model -> ( Model, Cmd Msg )
moveCursorColBy val model =
    let
        newCol =
            clamp 0
                (TextBuffer.lastColumn model.buffer model.cursor.row)
                (model.cursor.col + val)
    in
    ( { model
        | cursor = { row = model.cursor.row, col = newCol }
        , targetCol = newCol
      }
    , Cmd.none
    )


cursorLeft : Int -> Model -> ( Model, Cmd Msg )
cursorLeft lastColPrevRow model =
    let
        left =
            model.cursor.col - 1

        cursor =
            if left < 0 && model.cursor.row <= 0 then
                { row = 0, col = 0 }

            else if left < 0 then
                { row = model.cursor.row - 1, col = lastColPrevRow }

            else
                { row = model.cursor.row, col = left }
    in
    ( { model
        | cursor = cursor
        , targetCol = cursor.col
      }
    , Cmd.none
    )


cursorRight : Model -> ( Model, Cmd Msg )
cursorRight model =
    let
        right =
            model.cursor.col + 1

        rightMost =
            TextBuffer.lastColumn model.buffer model.cursor.row

        cursor =
            if right > rightMost && model.cursor.row >= (TextBuffer.length model.buffer - 1) then
                model.cursor

            else if right > rightMost then
                { row = model.cursor.row + 1, col = 0 }

            else
                { row = model.cursor.row, col = right }
    in
    ( { model
        | cursor = cursor
        , targetCol = cursor.col
      }
    , Cmd.none
    )


refocusBuffer : Model -> ( Model, Cmd Msg )
refocusBuffer model =
    ( { model | buffer = TextBuffer.refocus model.cursor.row model.cursor.col model.buffer }
    , Cmd.none
    )


scrollIfNecessary : Model -> ( Model, Cmd Msg )
scrollIfNecessary model =
    let
        ( newScrollRow, scrollCmd ) =
            if model.cursor.row > (model.scrollRow + model.linesPerPage - 3) then
                let
                    topRow =
                        min
                            (TextBuffer.lastLine model.buffer - model.linesPerPage + 1)
                            (model.cursor.row - model.linesPerPage + 3)
                in
                ( topRow, scrollTo ((topRow |> toFloat) * config.lineHeight - model.bottomOffset) )

            else if model.cursor.row < (model.scrollRow + 2) then
                let
                    topRow =
                        max
                            0
                            (model.cursor.row - 2)
                in
                ( topRow, scrollTo ((topRow |> toFloat) * config.lineHeight) )

            else
                ( model.scrollRow, Cmd.none )
    in
    ( { model | scrollRow = newScrollRow }
    , scrollCmd
    )


rippleBuffer : Model -> ( Model, Cmd Msg )
rippleBuffer model =
    ( { model | buffer = TextBuffer.rippleTo (model.scrollRow + model.linesPerPage) model.buffer }
    , Cmd.none
    )


insertChar : Char -> Model -> ( Model, Cmd Msg )
insertChar char model =
    ( { model | buffer = TextBuffer.insertCharAt char model.cursor.row model.cursor.col model.buffer }
    , Cmd.none
    )


newline : Model -> ( Model, Cmd Msg )
newline model =
    ( { model | buffer = TextBuffer.breakLine model.cursor.row model.cursor.col model.buffer }
    , Cmd.none
    )


backspace : Model -> ( Model, Cmd Msg )
backspace model =
    ( { model | buffer = TextBuffer.deleteCharBefore model.cursor.row model.cursor.col model.buffer }
    , Cmd.none
    )


delete : Model -> ( Model, Cmd Msg )
delete model =
    ( { model | buffer = TextBuffer.deleteCharAt model.cursor.row model.cursor.col model.buffer }
    , Cmd.none
    )


activity : Model -> ( Model, Cmd Msg )
activity model =
    ( { model | blinker = True }
    , Time.now |> Task.perform Activity
    )


{-| The difference between the height and the height floored to line height.
-}
bottomOffset : Float -> Float
bottomOffset height =
    height
        - (((height / config.lineHeight) |> floor |> toFloat) * config.lineHeight)


linesPerPage : Float -> Int
linesPerPage height =
    (height / config.lineHeight) |> floor


initEditorSize : Cmd Msg
initEditorSize =
    Browser.Dom.getViewportOf "editor-main" |> Task.attempt ContentViewPort


scrollTo : Float -> Cmd Msg
scrollTo pos =
    Browser.Dom.setViewportOf "editor-main" 0.0 pos |> Task.attempt (always NoOp)



-- Styling


global : List Css.Global.Snippet
global =
    [ Css.Global.html
        [ Css.pct 100 |> Css.height ]
    , Css.Global.body
        [ Css.pct 100 |> Css.height ]
    , Css.Global.id "editor-main"
        [ Css.position Css.relative
        , Css.fontFamily Css.monospace
        , Css.whiteSpace Css.pre
        , Css.overflowX Css.hidden
        , Css.overflowY Css.scroll
        , Css.pct 100 |> Css.width
        , Css.pct 100 |> Css.height
        ]
    , Css.Global.id "editor-main-inner"
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.outline Css.none
        ]
    , Css.Global.id "content-main"
        [ Css.position Css.relative
        , Css.property "flex" "1"
        , Css.property "user-select" "none"
        , Css.em 1 |> Css.marginLeft
        , Css.em 1 |> Css.marginRight
        ]
    , Css.Global.class "content-line"
        [ Css.position Css.absolute
        , Css.px 0 |> Css.left
        , Css.px 0 |> Css.right
        , Css.px config.lineHeight |> Css.lineHeight
        ]
    , Css.Global.class "cursors"
        [ Css.position Css.relative
        ]
    , Css.Global.class "cursor"
        [ Css.position Css.absolute
        , Css.px config.lineHeight |> Css.height
        , Css.borderLeft3 (Css.px 2.5) Css.solid (Css.rgb 90 95 167)
        ]
    ]



-- View


view : Model -> Document Msg
view model =
    { title = "Buffer Spike"
    , body =
        [ Css.Global.global global |> Html.Styled.toUnstyled
        , editorView model
        ]
    }


editorView : Model -> Html Msg
editorView model =
    H.div
        [ HA.id "editor-main"
        , HE.on "scroll" scrollDecoder
        , HE.preventDefaultOn "keydown" keyDecoder
        ]
        [ H.div
            [ HA.id "editor-main-inner"
            , HA.tabindex 0
            ]
            [ viewContent model
            ]
        ]


viewCursors : Model -> Html Msg
viewCursors model =
    H.div
        [ HA.class "cursors"
        , if model.blinker then
            HA.style "visibility" "visible"

          else
            HA.style "visibility" "hidden"
        ]
        [ viewCursor model ]


viewCursor : Model -> Html Msg
viewCursor model =
    let
        top =
            String.fromFloat
                (toFloat model.cursor.row * config.lineHeight)
                ++ "px"

        left =
            String.fromInt model.cursor.col ++ "ch"
    in
    H.div
        [ HA.class "cursor"
        , HA.style "top" top
        , HA.style "left" left
        ]
        [ H.text "" ]


viewContent : Model -> Html Msg
viewContent model =
    let
        pad =
            -- Ensure there is always 1 full page above and below for page up and down.
            model.linesPerPage + 1

        startLine =
            max 0
                ((model.top / config.lineHeight |> floor) - pad)

        endLine =
            ((model.top + model.height) / config.lineHeight |> floor) + pad

        height =
            (TextBuffer.length model.buffer |> toFloat) * config.lineHeight
    in
    H.div
        [ HA.id "content-main"
        , HA.style "height" (String.fromFloat height ++ "px")
        ]
        [ viewCursors model
        , keyedViewLines startLine endLine model.buffer
        ]


keyedViewLines : Int -> Int -> TextBuffer Tag Tag -> Html Msg
keyedViewLines start end buffer =
    List.range start end
        |> List.foldr
            (\idx accum ->
                case TextBuffer.getLine idx buffer of
                    Nothing ->
                        accum

                    Just row ->
                        viewKeyedLine idx row :: accum
            )
            []
        |> Keyed.node "div" []


viewKeyedLine : Int -> TextBuffer.Line Tag Tag -> ( String, Html Msg )
viewKeyedLine row content =
    ( String.fromInt row
    , Html.Lazy.lazy2 viewLine row content
    )


viewLine : Int -> TextBuffer.Line Tag Tag -> Html Msg
viewLine row line =
    let
        content =
            List.map
                (\( tag, str ) ->
                    case tag of
                        NormalText ->
                            H.span [ HA.style "color" "black" ] [ H.text str ]

                        QuotedText ->
                            H.span [ HA.style "color" "green" ] [ H.text str ]
                )
                line.tagged
    in
    H.div
        [ HA.class "content-line"
        , HA.style "top" (String.fromFloat (toFloat row * config.lineHeight) ++ "px")
        ]
        content



-- Scroll events


type alias ScrollEvent =
    { scrollTop : Float
    , scrollHeight : Float
    , scrollLeft : Float
    , scrollWidth : Float
    }


scrollDecoder : Decoder Msg
scrollDecoder =
    Decode.succeed ScrollEvent
        |> andMap (Decode.at [ "target", "scrollTop" ] Decode.float)
        |> andMap (Decode.at [ "target", "scrollHeight" ] Decode.float)
        |> andMap (Decode.at [ "target", "scrollLeft" ] Decode.float)
        |> andMap (Decode.at [ "target", "scrollWidth" ] Decode.float)
        |> Decode.map Scroll



-- Keyboard events.


type alias KeyEvent =
    { key : String
    , ctrlKey : Bool
    }


keyDecoder : Decoder ( Msg, Bool )
keyDecoder =
    Decode.succeed KeyEvent
        |> andMap (Decode.field "key" Decode.string)
        |> andMap (Decode.field "ctrlKey" Decode.bool)
        |> Decode.andThen keyToMsg


keyToMsg : KeyEvent -> Decoder ( Msg, Bool )
keyToMsg keyEvent =
    case String.uncons keyEvent.key of
        Just ( char, "" ) ->
            Decode.succeed ( InsertChar char, True )

        _ ->
            case keyEvent.key of
                "ArrowUp" ->
                    Decode.succeed ( MoveUp, True )

                "ArrowDown" ->
                    Decode.succeed ( MoveDown, True )

                "ArrowLeft" ->
                    Decode.succeed ( MoveLeft, True )

                "ArrowRight" ->
                    Decode.succeed ( MoveRight, True )

                "PageUp" ->
                    Decode.succeed ( PageUp, True )

                "PageDown" ->
                    Decode.succeed ( PageDown, True )

                "Backspace" ->
                    Decode.succeed ( RemoveCharBefore, True )

                "Delete" ->
                    Decode.succeed ( RemoveCharAfter, True )

                "Enter" ->
                    Decode.succeed ( NewLine, True )

                "Home" ->
                    if keyEvent.ctrlKey then
                        Decode.succeed ( FileHome, True )

                    else
                        Decode.succeed ( LineHome, True )

                "End" ->
                    if keyEvent.ctrlKey then
                        Decode.succeed ( FileEnd, True )

                    else
                        Decode.succeed ( LineEnd, True )

                _ ->
                    Decode.fail "This key does nothing"



-- Random buffer initialization.


randomBuffer : Int -> Int -> Generator (TextBuffer Tag Tag)
randomBuffer width length =
    let
        regex =
            Regex.fromString "(\\b[^\\s]+\\b)"
                |> Maybe.withDefault Regex.never

        wordList =
            Regex.find regex lorumIpsum
                |> List.map (.match >> String.toLower)
                |> Array.fromList

        wordGenerator =
            Random.Array.sample wordList

        line curLength curLine generator =
            if curLength >= width then
                String.concat curLine
                    |> Random.constant

            else
                generator
                    |> Random.andThen
                        (\randomWord ->
                            case randomWord of
                                Nothing ->
                                    -- This should not happen.
                                    line curLength curLine generator

                                Just val ->
                                    line (curLength + String.length val + 1)
                                        ((val ++ " ") :: curLine)
                                        generator
                        )
    in
    line 0 [] wordGenerator
        |> Random.Array.array length
        |> Random.map (TextBuffer.fromArray initialCtx tagLineFn)


lorumIpsum : String
lorumIpsum =
    """
  Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras ut feugiat orci. Ut cursus malesuada nunc id tempor. Nam dignissim luctus mi ac vestibulum. Fusce fermentum purus quis rutrum facilisis. Sed ut justo ac nulla ornare dictum. Ut condimentum pellentesque volutpat. Aliquam sapien eros, ornare eget nisi porta, mattis lobortis mauris. In hac habitasse platea dictumst. Aliquam scelerisque risus sed luctus accumsan.

Mauris posuere pellentesque urna, in consectetur enim tempor volutpat. Nulla convallis, turpis nec convallis eleifend, nisi elit vulputate nibh, nec cursus tellus purus eu mauris. Nulla facilisi. Ut placerat vulputate pharetra. Etiam libero est, eleifend quis semper ac, fringilla vitae arcu. Quisque ut cursus leo. Suspendisse augue tortor, venenatis at ex sit amet, fringilla malesuada ligula. Phasellus nulla nibh, mollis ut vulputate quis, congue vitae mi. Nunc porta, ex quis luctus scelerisque, eros mauris placerat mauris, vitae finibus leo nulla in lacus. Donec ac ex leo. Aliquam ut quam tincidunt, maximus neque et, hendrerit magna.

Integer elementum leo lacinia risus pharetra, sit amet condimentum felis porta. Phasellus sollicitudin mauris at risus semper, in facilisis ex dictum. Vestibulum tincidunt eros a vehicula dignissim. Quisque a ex et arcu bibendum congue. Praesent gravida nulla metus, sed luctus justo fermentum et. Nullam scelerisque, felis tempor placerat eleifend, neque risus dictum nisi, eget ullamcorper massa mauris non metus. Nam rhoncus mollis justo, eu luctus arcu pharetra et. Aenean auctor et massa tempus consectetur. Duis tempus nunc volutpat dolor pellentesque, non imperdiet purus sagittis. Sed ultricies neque vel condimentum tincidunt. Suspendisse ornare sodales risus, sed tincidunt tortor rutrum sed. Suspendisse pellentesque quis quam vel elementum.

Sed blandit orci ut lectus efficitur tempor. Maecenas vitae risus sodales leo fringilla posuere. Sed facilisis magna non eros porttitor, a molestie neque ultricies. Nulla ac sapien lacus. Aliquam erat volutpat. Etiam volutpat sem mauris, vitae luctus neque imperdiet non. Ut condimentum eget ipsum lobortis eleifend. Ut dignissim laoreet fringilla. Ut sit amet metus pharetra, malesuada diam vel, convallis ipsum.

Integer ac pellentesque turpis, id placerat libero. Fusce commodo mauris vitae augue laoreet, id dignissim enim placerat. Suspendisse et tellus semper, dictum nibh quis, tempus est. Etiam sagittis non lectus eu dapibus. Nullam metus nunc, lacinia ut varius et, commodo quis metus. In vel efficitur nisl. Mauris ac mi sed dolor scelerisque vulputate. In gravida urna ut tempus tempor. Quisque pulvinar velit ac lacus gravida vulputate. Integer ante odio, ultricies a posuere ut, sollicitudin ut risus.

Nullam volutpat consequat metus ac gravida. Curabitur iaculis nibh leo, non lacinia velit porta vitae. Aliquam convallis libero sed quam pharetra, eget cursus ex sagittis. Donec sodales in libero et finibus. Nunc rhoncus eleifend odio maximus sollicitudin. Fusce euismod erat quis enim cursus, eget imperdiet lectus rhoncus. Integer aliquet, nunc nec posuere condimentum, ex mauris fringilla urna, sit amet fermentum neque risus eu felis. Suspendisse tortor nibh, commodo et varius a, pulvinar vel urna. Nam porta aliquet egestas. Nam in fringilla ipsum. Praesent gravida nisl nec arcu pretium, pharetra vestibulum dolor placerat. Nullam rutrum in dolor ac mollis. Duis ornare laoreet enim.
  """



-- Helpers


randomToTask : Generator a -> Task x a
randomToTask generator =
    Time.now
        |> Task.map (Tuple.first << Random.step generator << Random.initialSeed << Time.posixToMillis)


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    Decode.map2 (|>)
