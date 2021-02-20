module Main exposing (main)

import Array exposing (Array)
import Browser as Browser
import Browser.Dom as Dom
import Html as H exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD exposing (Decoder)
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { line : String
    , cursor : Position
    , modifier : InputModifier
    }


type KeyboardEventType
    = KeyUp
    | KeyDown


type ModifierKeyType
    = Alt
    | Control
    | Meta
    | Shift
    | AltGraph


type alias InputModifier =
    { alt : Bool
    , control : Bool
    , meta : Bool
    , shift : Bool
    , altgr : Bool
    }


type alias Position =
    { column : Int
    }


type Msg
    = NoOp
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | MoveHome
    | MoveEnd
    | Tab
    | NewLine
    | InsertChar Char
    | InsertString String
    | RemoveCharBefore
    | RemoveCharAfter
    | ReleaseModifierKey ModifierKeyType
    | HoldModifierKey ModifierKeyType
    | Focus


initModel : Model
initModel =
    { line = ""
    , cursor = Position 0
    , modifier =
        { control = False
        , meta = False
        , shift = False
        , alt = False
        , altgr = False
        }
    }


init : flag -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Dom.focus "editor_hidden_input"
        |> Task.attempt (always NoOp)
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    H.div []
        [ viewEditor model
        ]


fontSize : Float
fontSize =
    20


lineHeight : Float
lineHeight =
    fontSize * 1.2


viewPrompt : Model -> Html Msg
viewPrompt model =
    H.div
        [ HA.style "width" "2em"
        , HA.style "text-align" "center"
        , HA.style "color" "#888"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        ]
        [ H.text ">" ]


compositionEndDecoder =
    JD.field "data" JD.string |> JD.map (\x -> { message = InsertString x, preventDefault = False, stopPropagation = False })


focusDecoder : Decoder ( Msg, Bool )
focusDecoder =
    JD.succeed ( Focus, True )


keyDecoder : KeyboardEventType -> InputModifier -> Decoder ( Msg, Bool )
keyDecoder kind withModifier =
    let
        alwaysPreventDefault : msg -> ( msg, Bool )
        alwaysPreventDefault msg =
            ( msg, True )

        msgDecoder : { isCompositing : Bool, key : String } -> Decoder Msg
        msgDecoder jd =
            case ( jd.isCompositing, kind ) of
                ( False, KeyUp ) ->
                    keyUpToMsg jd.key

                ( False, KeyDown ) ->
                    keyDownToMsg withModifier jd.key

                ( True, _ ) ->
                    JD.succeed NoOp
    in
    JD.map2 (\k isc -> { key = k, isCompositing = isc }) (JD.field "key" JD.string) (JD.field "isComposing" JD.bool)
        |> JD.andThen msgDecoder
        |> JD.map alwaysPreventDefault


keyDownToMsg : InputModifier -> String -> Decoder Msg
keyDownToMsg withPrefix string =
    case String.uncons string of
        Just ( char, "" ) ->
            let
                { meta, alt, shift, control, altgr } =
                    withPrefix
            in
            if meta || control then
                JD.succeed NoOp

            else
                JD.succeed (InsertChar char)

        _ ->
            case string of
                "ArrowUp" ->
                    JD.succeed MoveUp

                "ArrowDown" ->
                    JD.succeed MoveDown

                "ArrowLeft" ->
                    JD.succeed MoveLeft

                "ArrowRight" ->
                    JD.succeed MoveRight

                "Home" ->
                    JD.succeed MoveHome

                "End" ->
                    JD.succeed MoveEnd

                "Backspace" ->
                    JD.succeed RemoveCharBefore

                "Delete" ->
                    JD.succeed RemoveCharAfter

                "Enter" ->
                    JD.succeed NewLine

                "Tab" ->
                    JD.succeed Tab

                "Meta" ->
                    JD.succeed (HoldModifierKey Meta)

                "Alt" ->
                    JD.succeed (HoldModifierKey Alt)

                "Shift" ->
                    JD.succeed (HoldModifierKey Shift)

                "Control" ->
                    JD.succeed (HoldModifierKey Control)

                "AltGraph" ->
                    JD.succeed (HoldModifierKey AltGraph)

                _ ->
                    JD.fail "This key does nothing"


keyUpToMsg : String -> Decoder Msg
keyUpToMsg string =
    case string of
        "Meta" ->
            JD.succeed (ReleaseModifierKey Meta)

        "Alt" ->
            JD.succeed (ReleaseModifierKey Alt)

        "Shift" ->
            JD.succeed (ReleaseModifierKey Shift)

        "Control" ->
            JD.succeed (ReleaseModifierKey Control)

        "AltGraph" ->
            JD.succeed (ReleaseModifierKey AltGraph)

        _ ->
            JD.fail "This key does nothing"


viewEditor : Model -> Html Msg
viewEditor model =
    let
        { modifier } =
            model
    in
    H.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "row"
        , HA.style "font-family" "monospace"
        , HA.style "font-size" (String.fromFloat fontSize ++ "px")
        , HA.style "line-height" (String.fromFloat lineHeight ++ "px")
        , HA.style "white-space" "pre"
        , HA.tabindex 0
        , HA.id "editor"
        , HE.preventDefaultOn "click" focusDecoder
        ]
        [ viewPrompt model
        , viewContent model
        , H.div [ HA.attribute "style" "overflow:hidden; height:0; outline: none; position: fixed; top:0" ]
            [ H.input
                [ HA.id "editor_hidden_input"
                , HE.custom "compositionend" compositionEndDecoder
                , HE.preventDefaultOn "keydown" (keyDecoder KeyDown modifier)
                , HE.preventDefaultOn "keyup" (keyDecoder KeyUp modifier)
                ]
                []
            ]
        ]


viewContent : Model -> Html Msg
viewContent model =
    H.div
        [ HA.style "position" "relative"
        , HA.style "flex" "1"
        , HA.style "background-color" "#f0f0f0"
        , HA.style "user-select" "none"
        ]
        [ viewLine model.cursor model.line ]


nbsp : String
nbsp =
    "\u{00A0}"


viewCursor : Position -> String -> Html Msg
viewCursor position char =
    H.span
        [ HA.style "background-color" "orange"
        ]
        [ H.text char ]


viewLine : Position -> String -> Html Msg
viewLine position content =
    H.div
        [ HA.style "position" "absolute"
        , HA.style "left" "0"
        , HA.style "right" "0"
        , HA.style "height" (String.fromFloat lineHeight ++ "px")
        , HA.style "top" "0"
        ]
        (if isLastColumn content position.column then
            viewChars position
                content
                ++ [ viewCursor position nbsp ]

         else
            viewChars position content
        )


isLastColumn : String -> Int -> Bool
isLastColumn content pos =
    pos == String.length content


viewChars : Position -> String -> List (Html Msg)
viewChars position content =
    List.indexedMap (\i x -> viewChar position i x) (String.toList content)


viewChar : Position -> Int -> Char -> Html Msg
viewChar position column char =
    if position.column == column then
        viewCursor
            position
            (String.fromChar char)

    else
        H.span
            []
            [ H.text (String.fromChar char) ]


firstLeftNonSpace : String -> Int -> Int
firstLeftNonSpace content cursor =
    -- find first non space character after a space before current position
    let
        subcontent =
            String.toList (String.slice 0 (cursor - 1) content)

        pos =
            Tuple.second
                (List.foldl
                    (\char acc ->
                        let
                            dbg =
                                Debug.log "acc" ( char, acc )
                        in
                        if char == ' ' then
                            ( Tuple.first acc + 1, Tuple.first acc + 1 )

                        else
                            ( Tuple.first acc + 1, Tuple.second acc )
                    )
                    ( -1, -1 )
                    subcontent
                )
    in
    pos + 1


firstRightNonSpace : String -> Int -> Int
firstRightNonSpace content cursor =
    -- find first non space character before a space after current postiion
    let
        subcontent =
            String.toList (String.slice 0 (String.length content - cursor) (String.reverse content))

        pos =
            Tuple.second
                (List.foldl
                    (\char acc ->
                        let
                            dbg =
                                Debug.log "acc" ( char, acc )
                        in
                        if char == ' ' then
                            ( Tuple.first acc + 1, Tuple.first acc + 1 )

                        else
                            ( Tuple.first acc + 1, Tuple.second acc )
                    )
                    ( -1, -1 )
                    subcontent
                )
    in
    min (String.length content) (String.length content - pos)


moveLeft : Position -> String -> InputModifier -> Position
moveLeft ({ column } as position) line modifier =
    if column == 0 then
        position

    else if modifier.control then
        { column = firstLeftNonSpace line column }

    else
        { column = column - 1 }


moveRight : Position -> String -> InputModifier -> Position
moveRight ({ column } as position) line modifier =
    if column == String.length line then
        position

    else if modifier.control then
        { column = firstRightNonSpace line column }

    else
        { column = column + 1 }


insertChar : Char -> Model -> Model
insertChar char ({ cursor, line } as model) =
    let
        { column } =
            cursor

        lineWithCharAdded : String -> String
        lineWithCharAdded content =
            String.left column content
                ++ String.fromChar char
                ++ String.dropLeft column content

        newLine =
            lineWithCharAdded line

        newCursor : Position
        newCursor =
            { column = column + 1
            }
    in
    { model
        | line = newLine
        , cursor = newCursor
    }


insertString : String -> Model -> Model
insertString string ({ cursor, line } as model) =
    let
        { column } =
            cursor

        lineWithCharAdded : String -> String
        lineWithCharAdded content =
            String.left column content
                ++ string
                ++ String.dropLeft column content

        newLine =
            lineWithCharAdded line

        newCursor : Position
        newCursor =
            { column = column + String.length string
            }
    in
    { model
        | line = newLine
        , cursor = newCursor
    }


removeCharBefore : Model -> Model
removeCharBefore ({ cursor, line } as model) =
    let
        { column } =
            cursor
    in
    if column == 0 then
        model

    else
        let
            removeCharFromLine : String -> String
            removeCharFromLine content =
                if column == 0 then
                    ""

                else
                    String.left (column - 1) content
                        ++ String.dropLeft column content

            newLine =
                removeCharFromLine line
        in
        { model
            | line = newLine
            , cursor = moveLeft cursor line model.modifier
        }


removeCharAfter : Model -> Model
removeCharAfter ({ cursor, line } as model) =
    let
        { column } =
            cursor
    in
    if column == String.length line then
        model

    else
        let
            isOnLastColumn : Bool
            isOnLastColumn =
                column == String.length line

            removeCharFromLine : String -> String
            removeCharFromLine content =
                String.left column content
                    ++ String.dropLeft (column + 1) content

            newLine : String
            newLine =
                removeCharFromLine line
        in
        { model
            | line = newLine
            , cursor = cursor
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Focus ->
            ( model
            , Dom.focus "editor_hidden_input"
                |> Task.attempt (always NoOp)
            )

        NoOp ->
            ( model, Cmd.none )

        MoveUp ->
            ( model
            , Cmd.none
            )

        MoveDown ->
            ( model
            , Cmd.none
            )

        MoveLeft ->
            ( { model | cursor = moveLeft model.cursor model.line model.modifier }
            , Cmd.none
            )

        MoveHome ->
            ( { model | cursor = { column = 0 } }
            , Cmd.none
            )

        MoveEnd ->
            ( { model | cursor = { column = String.length model.line } }
            , Cmd.none
            )

        MoveRight ->
            ( { model | cursor = moveRight model.cursor model.line model.modifier }
            , Cmd.none
            )

        Tab ->
            ( insertChar '\t' model, Cmd.none )

        NewLine ->
            ( model
            , Cmd.none
            )

        InsertChar char ->
            ( insertChar char model
            , Cmd.none
            )

        InsertString string ->
            ( insertString string model
            , Cmd.none
            )

        RemoveCharBefore ->
            ( removeCharBefore model
            , Cmd.none
            )

        RemoveCharAfter ->
            ( removeCharAfter model
            , Cmd.none
            )

        HoldModifierKey key ->
            let
                previous =
                    model.modifier
            in
            case key of
                Meta ->
                    ( { model | modifier = { previous | meta = True } }, Cmd.none )

                Alt ->
                    ( { model | modifier = { previous | alt = True } }, Cmd.none )

                Shift ->
                    ( { model | modifier = { previous | shift = True } }, Cmd.none )

                Control ->
                    ( { model | modifier = { previous | control = True } }, Cmd.none )

                AltGraph ->
                    ( { model | modifier = { previous | altgr = True } }, Cmd.none )

        ReleaseModifierKey key ->
            let
                previous =
                    model.modifier
            in
            case key of
                Meta ->
                    ( { model | modifier = { previous | meta = False } }, Cmd.none )

                Alt ->
                    ( { model | modifier = { previous | alt = False } }, Cmd.none )

                Shift ->
                    ( { model | modifier = { previous | shift = False } }, Cmd.none )

                Control ->
                    ( { model | modifier = { previous | control = False } }, Cmd.none )

                AltGraph ->
                    ( { model | modifier = { previous | altgr = False } }, Cmd.none )
