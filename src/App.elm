module App exposing (..)

import Html exposing (Html, div, p, h1, text)
import Html.Attributes exposing (class)
import Navigation exposing (Location)
import UrlParser exposing (Parser, (</>), int, oneOf, map, s, string)
import OAuth
import OAuth.Implicit
import Routing exposing (..)
import Http
import User


authorizationEndpoint : String
authorizationEndpoint =
    "https://accounts.google.com/o/oauth2/v2/auth"


profileEndpoint : String
profileEndpoint =
    "https://www.googleapis.com/oauth2/v1/userinfo"


type alias OAuth =
    { clientId : String
    , redirectUrl : String
    , token : Maybe OAuth.Token
    }


type alias Model =
    { i : Int
    , route : Page
    , oauth : OAuth
    , error : Maybe String
    , user : User.Model
    }


type alias Flags =
    { clientId : String }


type Msg
    = UrlChange Location
    | NoOp
    | GetProfile (Result Http.Error User.Profile)
    | UserMsg User.Msg


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        model =
            { i = 0
            , oauth =
                { clientId = flags.clientId
                , redirectUrl = location.origin ++ location.pathname
                , token = Nothing
                }
            , error = Nothing
            , route = Home
            , user = { profile = Nothing }
            }
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
                            , expect = Http.expectJson User.profilerDecoder
                            , timeout = Nothing
                            }

                    oauth =
                        model.oauth
                in
                    { model | oauth = { oauth | token = Just token } }
                        ! [ Navigation.modifyUrl model.oauth.redirectUrl
                          , Http.send GetProfile req
                          ]

            Err (OAuth.Empty) ->
                ( model
                , OAuth.Implicit.authorize
                    { clientId = model.oauth.clientId
                    , redirectUri = model.oauth.redirectUrl
                    , responseType = OAuth.Token
                    , scope = [ "email", "profile" ]
                    , state = Nothing
                    , url = authorizationEndpoint
                    }
                )

            Err (OAuth.OAuthErr err) ->
                { model | error = Just <| OAuth.showErrCode err.error }
                    ! [ Navigation.modifyUrl model.oauth.redirectUrl ]

            Err _ ->
                model ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            urlUpdate location model

        NoOp ->
            model ! []

        GetProfile res ->
            case res of
                Err _ ->
                    { model | error = Just "Unable to fetch profile" } ! []

                Ok _ ->
                    let
                        ( userModel, cmd ) =
                            User.update (User.GetProfile res) model.user
                    in
                        { model | user = userModel }
                            ! [ Cmd.none ]

        UserMsg subMsg ->
            let
                ( userModel, userCmd ) =
                    User.update subMsg model.user
            in
                { model | user = userModel } ! [ Cmd.map UserMsg userCmd ]


urlUpdate : Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    case (hashParser location) of
        Just page ->
            updatePage page model

        Nothing ->
            updatePage Home model


updatePage : Page -> Model -> ( Model, Cmd Msg )
updatePage page model =
    ( { model
        | route = page
      }
    , updatePageMessage model page
    )


updatePageMessage : Model -> Page -> Cmd Msg
updatePageMessage model page =
    case page of
        Home ->
            Cmd.none

        RecipeView id ->
            Cmd.none

        RecipeEdit id ->
            Cmd.none

        RecipeCreate ->
            Cmd.none


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ map Home (s "")
        , map RecipeEdit (s "recipe" </> s "edit" </> int)
        , map RecipeCreate (s "createrecipe")
        , map RecipeView (s "recipe" </> int)
        ]


hashParser : Location -> Maybe Page
hashParser location =
    UrlParser.parseHash pageParser location


view : Model -> Html Msg
view model =
    div
        []
        [ div
            [ class "header" ]
            [ h1 [ class "title" ] [ text "RecipeDB" ]
            , Html.map UserMsg (User.view model.user)
            ]
        , div
            [ class "content" ]
            [ div
                [ class "boxwrapper" ]
                [ div
                    [ class "box" ]
                    [ text "1" ]
                , div
                    [ class "box" ]
                    [ text "2" ]
                , div
                    [ class "box" ]
                    [ text "3" ]
                , div
                    [ class "box" ]
                    [ text "4" ]
                , div
                    [ class "box" ]
                    [ text "5" ]
                , div
                    [ class "box" ]
                    [ text "6" ]
                , div
                    [ class "box" ]
                    [ text "7" ]
                , div
                    [ class "box" ]
                    [ text "8" ]
                , div
                    [ class "box" ]
                    [ text "9" ]
                , div
                    [ class "box" ]
                    [ text "10" ]
                ]
            ]
        ]


main : Program Flags Model Msg
main =
    Navigation.programWithFlags
        UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }
