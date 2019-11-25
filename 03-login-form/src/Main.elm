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
import Html.Events exposing (onClick, onInput, onSubmit)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , age : String
    , validationMessage : Html Msg
    }


init : Model
init =
    Model "" "" "" "" <| div [] []


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }

        Age age ->
            { model | age = age }

        Submit ->
            { model | validationMessage = viewValidation model }


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password"
            "Re-enter Password"
            model.passwordAgain
            PasswordAgain
        , viewInput "age" "Age" model.age Age
        , model.validationMessage
        , button [ onClick Submit ] [ text "Submit" ]
        ]


viewInput : String -> String -> String -> (String -> Msg) -> Html Msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    div []
        [ br [] []
        , viewPasswordValidation model
        , br [] []
        , viewAgeValidation model
        , br [] []
        ]


viewPasswordValidation : Model -> Html msg
viewPasswordValidation model =
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
        div [ style "color" "green" ] [ text "Password is OK" ]


viewAgeValidation : Model -> Html msg
viewAgeValidation model =
    case String.toInt <| String.trim model.age of
        Nothing ->
            div [ style "color" "red" ] [ text "Age is not a number!" ]

        Just n ->
            if n < 0 then
                div [ style "color" "red" ] [ text "Age is below 0!" ]

            else
                div [ style "color" "green" ] [ text "Age is OK" ]


passesPasswordPolicy : String -> Bool
passesPasswordPolicy password =
    let
        filters =
            [ Char.isUpper, Char.isLower, Char.isDigit ]

        parts =
            List.map (\f -> String.filter f password) filters
    in
    List.all (not << String.isEmpty) parts
