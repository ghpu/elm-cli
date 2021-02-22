module Main exposing (main)

import Array exposing (Array)
import Browser as Browser
import Browser.Dom as Dom
import Cli exposing (Model, Msg(..), initModel, update, view)
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
    { climodel : Cli.Model
    }


type Msg
    = NoOp
    | CliMsg Cli.Msg


init : flag -> ( Model, Cmd Msg )
init _ =
    ( { climodel = Cli.initModel }
    , Dom.focus "editor_hidden_input"
        |> Task.attempt (always NoOp)
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    H.div []
        [ H.map toMsg (Cli.view model.climodel)
        ]


toMsg : Cli.Msg -> Msg
toMsg climsg =
    CliMsg climsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CliMsg what ->
            let
                ( climodel, cmd ) =
                    Cli.update what model.climodel
            in
            ( { model | climodel = climodel }, Cmd.map toMsg cmd )
