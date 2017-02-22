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
            "Network Error"

        Http.BadUrl s ->
            "BadURL Error: " ++ s

        Http.BadStatus r ->
            "BadStatus Error: " ++ (statusToString r)

        Http.BadPayload s r ->
            "BadPayload Error: " ++ (statusToString r)


statusToString : Http.Response String -> String
statusToString r =
    " ("
        ++ (toString r.status.code)
        ++ ", "
        ++ r.status.message
        ++ ")"


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
