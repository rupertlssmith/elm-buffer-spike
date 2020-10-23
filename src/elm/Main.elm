module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Dom exposing (Viewport)
import Browser.Events
import Css
import Css.Global
import GapBuffer exposing (Buffer)
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
    { buffer : Buffer String (Buffer Char Char)
    , top : Float
    , height : Float
    , cursor : RowCol
    , scrollRow : Int
    , linesPerPage : Int
    , bottomOffset : Float
    , blinker : Bool
    , lastActive : Posix
    }


type alias TextBuffer =
    Buffer String (Buffer Char Char)


type alias RowCol =
    { row : Int
    , col : Int
    }


init _ =
    ( { buffer = GapBuffer.empty stringToCharBuffer charBufferToString
      , top = 0
      , height = 0
      , cursor = { row = 0, col = 0 }
      , scrollRow = 0
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


subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize (\_ _ -> Resize)
        , Time.every config.blinkInterval Blink
        ]


type Msg
    = Scroll ScrollEvent
    | RandomBuffer TextBuffer
    | ContentViewPort (Result Browser.Dom.Error Viewport)
    | Resize
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | PageUp
    | PageDown
    | Blink Posix
    | Activity Posix
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomBuffer buffer ->
            ( { model | buffer = buffer }, Cmd.none )

        Scroll scroll ->
            ( { model | top = scroll.scrollTop }, Cmd.none )

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
                |> andThen refocusBuffer
                |> andThen scrollIfNecessary
                |> andThen activity

        MoveDown ->
            ( model, Cmd.none )
                |> andThen (moveCursorRowBy 1)
                |> andThen refocusBuffer
                |> andThen scrollIfNecessary
                |> andThen activity

        MoveLeft ->
            ( model, Cmd.none )
                |> andThen (moveCursorColBy -1)
                |> andThen refocusBuffer
                |> andThen activity

        MoveRight ->
            ( model, Cmd.none )
                |> andThen (moveCursorColBy 1)
                |> andThen refocusBuffer
                |> andThen activity

        PageUp ->
            ( model, Cmd.none )
                |> andThen (moveCursorRowBy -model.linesPerPage)
                |> andThen refocusBuffer
                |> andThen scrollIfNecessary
                |> andThen activity

        PageDown ->
            ( model, Cmd.none )
                |> andThen (moveCursorRowBy model.linesPerPage)
                |> andThen refocusBuffer
                |> andThen scrollIfNecessary
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


moveCursorRowBy : Int -> Model -> ( Model, Cmd Msg )
moveCursorRowBy val model =
    let
        newRow =
            clamp
                0
                (GapBuffer.length model.buffer - 1)
                (model.cursor.row + val)
    in
    ( { model | cursor = { row = newRow, col = model.cursor.col } }
    , Cmd.none
    )


moveCursorColBy : Int -> Model -> ( Model, Cmd Msg )
moveCursorColBy val model =
    let
        newCol =
            max 0 (model.cursor.col + val)
    in
    ( { model | cursor = { row = model.cursor.row, col = newCol } }
    , Cmd.none
    )


refocusBuffer : Model -> ( Model, Cmd Msg )
refocusBuffer model =
    let
        refocussedBuffer =
            GapBuffer.updateFocus model.cursor.row
                (\rowBuffer -> GapBuffer.updateFocus model.cursor.col identity rowBuffer)
                model.buffer
    in
    ( { model | buffer = refocussedBuffer }
    , Cmd.none
    )


scrollIfNecessary : Model -> ( Model, Cmd Msg )
scrollIfNecessary model =
    let
        ( newScrollRow, scrollCmd ) =
            if model.cursor.row > (model.scrollRow + model.linesPerPage - 3) then
                let
                    topRow =
                        model.cursor.row - model.linesPerPage + 3
                in
                ( topRow, scrollTo ((topRow |> toFloat) * config.lineHeight - model.bottomOffset) )

            else if model.cursor.row < (model.scrollRow + 2) then
                let
                    topRow =
                        model.cursor.row - 2
                in
                ( topRow, scrollTo ((topRow |> toFloat) * config.lineHeight) )

            else
                ( model.scrollRow, Cmd.none )
    in
    ( { model | scrollRow = newScrollRow }
    , scrollCmd
    )


activity : Model -> ( Model, Cmd Msg )
activity model =
    ( { model | blinker = True }, Time.now |> Task.perform Activity )


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
                (toFloat model.cursor.row
                    * config.lineHeight
                    - (config.lineHeight - config.fontSize)
                    / 2
                )
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
            (GapBuffer.length model.buffer |> toFloat) * config.lineHeight
    in
    H.div
        [ HA.id "content-main"
        , HA.style "height" (String.fromFloat height ++ "px")
        ]
        [ viewCursors model
        , keyedViewLines startLine endLine model.buffer
        ]


keyedViewLines : Int -> Int -> TextBuffer -> Html Msg
keyedViewLines start end buffer =
    List.range start end
        |> List.foldr
            (\idx accum ->
                case GapBuffer.get idx buffer of
                    Nothing ->
                        accum

                    Just row ->
                        viewKeyedLine idx row :: accum
            )
            []
        |> Keyed.node "div" []


viewLine : Int -> String -> Html Msg
viewLine row content =
    H.div
        [ HA.class "content-line"
        , HA.style "top" (String.fromFloat (toFloat row * config.lineHeight) ++ "px")
        ]
        [ H.text content ]


viewKeyedLine : Int -> String -> ( String, Html Msg )
viewKeyedLine row content =
    ( String.fromInt row
    , Html.Lazy.lazy2 viewLine row content
    )



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


keyDecoder : Decoder ( Msg, Bool )
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen keyToMsg


keyToMsg : String -> Decoder ( Msg, Bool )
keyToMsg string =
    case string of
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

        _ ->
            Decode.fail "This key does nothing"



-- Random buffer initialization.


randomBuffer : Int -> Int -> Generator TextBuffer
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
        |> Random.map (GapBuffer.fromArray stringToCharBuffer charBufferToString)


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
