module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, map2, string)


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
    | Success String String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getRandomAircraftGif )


type Msg
    = MorePlease
    | GotGif (Result Http.Error ( String, String ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getRandomAircraftGif )

        GotGif result ->
            case result of
                Ok ( url, title ) ->
                    ( Success url title, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Random Aircraft" ]
        , viewGif model
        ]


viewGif : Model -> Html Msg
viewGif model =
    case model of
        Failure ->
            div []
                [ text "I could not load a random aircraft for some reason. "
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success url title ->
            div []
                [ button
                    [ onClick MorePlease
                    , style "display" "block"
                    ]
                    [ text "More Please!" ]
                , h2 [] [ text title ]
                , img [ src url ] []
                ]


getRandomAircraftGif : Cmd Msg
getRandomAircraftGif =
    let
        urlBase =
            "https://api.giphy.com/v1/gifs/random"

        apiKey =
            "?api_key=b7ivRILeTIwkxytPKhO0mw31MBRDDpIW"

        tag =
            "&tag=aircraft"
    in
    Http.get
        { url = urlBase ++ apiKey ++ tag
        , expect = Http.expectJson GotGif gifDecoder
        }


gifDecoder : Decoder ( String, String )
gifDecoder =
    map2 (\x y -> ( x, y ))
        (field
            "data"
            (field "image_url" string)
        )
        (field
            "data"
            (field "title" string)
        )
