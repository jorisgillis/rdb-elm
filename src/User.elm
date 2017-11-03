module User exposing (..)

import OAuth
import OAuth.Implicit
import Json.Decode as Json
import Http
import Navigation
import Html exposing (Html, p, text)


authorizationEndpoint : String
authorizationEndpoint =
    "https://accounts.google.com/o/oauth2/v2/auth"


profileEndpoint : String
profileEndpoint =
    "https://www.googleapis.com/oauth2/v1/userinfo"


type alias Model =
    { oauth :
        { clientId : String
        , redirectUri : String
        }
    , error : Maybe String
    , token : Maybe OAuth.Token
    , profile : Maybe Profile
    }


type alias Profile =
    { email : String
    , name : String
    }


type Msg
    = Nop
    | GetProfile (Result Http.Error Profile)


initialModel : String -> Navigation.Location -> Model
initialModel clientId location =
    { oauth =
        { clientId = clientId
        , redirectUri = location.origin ++ location.pathname
        }
    , error = Nothing
    , token = Nothing
    , profile = Nothing
    }


init : String -> Navigation.Location -> ( Model, Cmd Msg )
init clientId location =
    let
        model =
            initialModel clientId location
    in
        case OAuth.Implicit.parse location of
            Ok { token } ->
                let
                    req =
                        Http.request
                            { method = "GET"
                            , body = Http.emptyBody
                            , headers = OAuth.use token []
                            , withCredentials = False
                            , url = profileEndpoint
                            , expect = Http.expectJson profileDecoder
                            , timeout = Nothing
                            }
                in
                    Debug.log "OK: "
                        ( { model | token = Just token }
                        , Cmd.batch
                            [ Navigation.modifyUrl model.oauth.redirectUri
                            , Http.send GetProfile req
                            ]
                        )

            Err (OAuth.Empty) ->
                Debug.log "Error OAuth.Empty "
                    ( model
                    , OAuth.Implicit.authorize
                        { clientId = model.oauth.clientId
                        , redirectUri = model.oauth.redirectUri
                        , responseType = OAuth.Token
                        , scope = [ "email", "profile" ]
                        , state = Nothing
                        , url = authorizationEndpoint
                        }
                    )

            Err (OAuth.OAuthErr err) ->
                Debug.log "Error OAuthErr"
                    ( { model | error = Just <| OAuth.showErrCode err.error }
                    , Navigation.modifyUrl model.oauth.redirectUri
                    )

            Err _ ->
                Debug.log "Error: other"
                    ( { model | error = Just "parsing error" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ oauth } as model) =
    case msg of
        Nop ->
            ( model, Cmd.none )

        GetProfile res ->
            case res of
                Err err ->
                    ( { model | error = Just "unable to fetch user profile ¯\\_(ツ)_/¯" }
                    , Cmd.none
                    )

                Ok profile ->
                    ( { model | profile = Just profile }
                    , Cmd.none
                    )


greeting : Model -> Html Msg
greeting model =
    case model.profile of
        Just profile ->
            p [] [ text <| "Hello, " ++ profile.name ++ "!" ]

        Nothing ->
            p [] [ text "Hello, Stranger!" ]


profileDecoder : Json.Decoder Profile
profileDecoder =
    Json.map2 Profile
        (Json.field "email" Json.string)
        (Json.field "name" Json.string)
