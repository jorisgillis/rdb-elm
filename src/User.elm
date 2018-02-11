module User exposing (..)

import Http
import Json.Decode as Json
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)


type alias Model =
    { profile : Maybe Profile }


type alias Profile =
    { email : String
    , name : String
    }


type Msg
    = GetProfile (Result Http.Error Profile)


profilerDecoder : Json.Decoder Profile
profilerDecoder =
    Json.map2 Profile
        (Json.field "email" Json.string)
        (Json.field "name" Json.string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetProfile res ->
            case res of
                Err _ ->
                    model ! []

                Ok profile ->
                    { model | profile = Just profile } ! []


view : Model -> Html Msg
view model =
    p [] [ text (greeting model) ]


greeting : Model -> String
greeting model =
    case model.profile of
        Just profile ->
            "Welcome " ++ profile.name

        Nothing ->
            "Welcome stranger"
