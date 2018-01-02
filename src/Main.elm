module App exposing (..)

import Html exposing (Html, div, text, ul, li, p, a, h1)
import Html.Attributes exposing (class, style)
import Navigation exposing (Location)
import UrlParser exposing (Parser, (</>), int, oneOf, map, s, string)
import Material
import Material.Layout as Layout
import Routing exposing (..)
import RecipeList
import RecipeView
import RecipeEdit
import RecipeModel exposing (RecipeId, RecipeModel, newRecipe)
import User
import HttpAuth


type alias Model =
    { recipes : RecipeList.Model
    , recipe : RecipeModel
    , route : Page
    , mdl : Material.Model
    , user : User.Model
    , auth : HttpAuth.HttpAuthModel
    }


type alias Flags =
    { clientId : String }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        ( userModel, userCmd ) =
            Debug.log "User Model"
                (User.init flags.clientId location)

        ( initializedModel, pageCmd ) =
            urlUpdate location
                { recipes = RecipeList.initialModel
                , recipe = RecipeModel.initialModel
                , route = Home
                , mdl = Material.model
                , user = userModel
                , auth = HttpAuth.initModel userModel.token
                }
    in
        ( initializedModel
        , Cmd.batch
            [ Cmd.map UserMsg userCmd
            , pageCmd
            ]
        )


type Msg
    = RecipeListMsg RecipeList.Msg
    | RecipeViewMsg RecipeView.Msg
    | RecipeEditMsg RecipeEdit.Msg
    | UserMsg User.Msg
    | GoHome
    | Mdl (Material.Msg Msg)
    | SelectTab Int
    | UrlChange Location


view : Model -> Html Msg
view model =
    renderMainLayout model


renderMainLayout : Model -> Html Msg
renderMainLayout model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.onSelectTab SelectTab
        ]
        { header =
            [ h1
                [ style [ ( "padding-left", "20px" ) ] ]
                [ text "RecipeDB" ]
            ]
        , drawer = []
        , tabs =
            ( [ text "Home"
              , text "Add Recipe"
              ]
            , []
            )
        , main =
            [ Html.map UserMsg (User.greeting model.user)
            , loadPage model
            ]
        }


loadPage : Model -> Html Msg
loadPage model =
    case model.route of
        Home ->
            (Html.map RecipeListMsg (RecipeList.view model.recipes))

        RecipeView _ ->
            (Html.map RecipeViewMsg (RecipeView.view model.recipe))

        RecipeEdit _ ->
            (Html.map RecipeEditMsg (RecipeEdit.view model.recipe))

        RecipeCreate ->
            (Html.map RecipeEditMsg (RecipeEdit.view model.recipe))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RecipeListMsg subMsg ->
            let
                ( newRecipes, cmd ) =
                    RecipeList.update subMsg model.recipes
            in
                ( { model | recipes = newRecipes }, Cmd.map RecipeListMsg cmd )

        RecipeViewMsg subMsg ->
            let
                ( newRecipe, cmd ) =
                    RecipeView.update subMsg model.recipe
            in
                ( { model | recipe = newRecipe }, Cmd.map RecipeViewMsg cmd )

        RecipeEditMsg subMsg ->
            let
                ( newRecipe, cmd ) =
                    RecipeEdit.update subMsg model.recipe
            in
                ( { model | recipe = newRecipe }, Cmd.map RecipeEditMsg cmd )

        UserMsg subMsg ->
            let
                ( newUser, cmd ) =
                    User.update subMsg model.user
            in
                ( { model | user = newUser }, Cmd.map UserMsg cmd )

        GoHome ->
            ( model, Navigation.newUrl (Routing.toHash Routing.Home) )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        SelectTab num ->
            case num of
                0 ->
                    ( model, Navigation.newUrl (Routing.toHash Routing.Home) )

                1 ->
                    ( model, Navigation.newUrl (Routing.toHash Routing.RecipeCreate) )

                _ ->
                    ( model, Navigation.newUrl (Routing.toHash Routing.Home) )

        UrlChange location ->
            urlUpdate location model


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
        , recipe = newRecipeOnCreate page model.recipe
      }
    , updatePageMessage model page
    )


newRecipeOnCreate : Page -> RecipeModel -> RecipeModel
newRecipeOnCreate page model =
    if page == RecipeCreate then
        { model | recipe = (newRecipe) }
    else
        model


updatePageMessage : Model -> Page -> Cmd Msg
updatePageMessage model page =
    case page of
        Home ->
            Cmd.map RecipeListMsg (RecipeList.fetchAll model.auth)

        RecipeView id ->
            Cmd.map RecipeViewMsg (RecipeView.fetchRecipe id)

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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Flags Model Msg
main =
    Navigation.programWithFlags
        UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
