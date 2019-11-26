module Main exposing (main)

import Browser
import Html exposing (Html, pre, text)
import Http


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Model
    = Failure
    | Loading
    | Success String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url =
            "https://gist.githubusercontent.com/tombusby/"
                ++ "613ee78eb5c3bcf01844f0b982f50c88/raw/"
                ++ "2e4e532e51fab869b5f12b07556364be1b673962/personnummer.rb"
        , expect = Http.expectString GotText
        }
    )


type Msg
    = GotText (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success fullText, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "I was unable to load your book."

        Loading ->
            text "Loading..."

        Success fullText ->
            pre [] [ text fullText ]
