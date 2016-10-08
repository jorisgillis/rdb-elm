module ErrorHandling exposing (errorToString, showError)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Http


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.UnexpectedPayload e ->
            "UnexpectedPayload: " ++ e

        Http.BadResponse code e ->
            "BadResponse: " ++ (toString code) ++ " " ++ e


showError : Maybe String -> Html a
showError error =
    case error of
        Just errorMsg ->
            div [ class "panel panel-danger" ]
                [ div [ class "panel-heading" ]
                    [ p [ class "panel-title" ] [ text "Error" ] ]
                , div [ class "panel-body" ] [ text errorMsg ]
                ]

        Nothing ->
            div [] []
