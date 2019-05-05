module Main exposing
    ( Model
    , Msg(..)
    , init
    , main
    , update
    , view
    , viewInput
    , viewValidation
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    }


init : Model
init =
    Model "" "" ""


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password"
            "Re-enter Password"
            model.passwordAgain
            PasswordAgain
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if model.password /= model.passwordAgain then
        div [ style "color" "red" ] [ text "Passwords do not match!" ]

    else if String.length model.password < 8 then
        div [ style "color" "red" ] [ text "Password is less then 8 chars!" ]

    else if not <| passesPasswordPolicy model.password then
        div [ style "color" "red" ]
            [ text
                "Password must contain upper, lower and numeric characters!"
            ]

    else
        div [ style "color" "green" ] [ text "OK" ]


passesPasswordPolicy : String -> Bool
passesPasswordPolicy password =
    let
        filters =
            [ Char.isUpper, Char.isLower, Char.isDigit ]

        parts =
            List.map (\f -> String.filter f password) filters
    in
    List.all (not << String.isEmpty) parts
