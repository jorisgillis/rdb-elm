module App exposing (..)

import Html exposing (Html, div, text, ul, li, p, a, h1)
import Html.App
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import String
import Navigation exposing (program)
import UrlParser exposing (Parser, (</>), format, int, oneOf, s, string)
import Material
import Material.Layout as Layout
import Routing exposing (..)
import RecipeList
import RecipeView
import RecipeEdit
import RecipeModel exposing (RecipeId, RecipeModel, newRecipe)


type alias Model =
    { recipes : RecipeList.Model
    , recipe : RecipeModel
    , route : Page
    , mdl : Material.Model
    }


init : Result String Page -> ( Model, Cmd Msg )
init result =
    urlUpdate result initialModel


initialModel : Model
initialModel =
    { recipes = RecipeList.initialModel
    , recipe = RecipeModel.initialModel
    , route = Home
    , mdl = Material.model
    }


type Msg
    = RecipeListMsg RecipeList.Msg
    | RecipeViewMsg RecipeView.Msg
    | RecipeEditMsg RecipeEdit.Msg
    | GoHome
    | Mdl (Material.Msg Msg)
    | SelectTab Int


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.onSelectTab SelectTab
        ]
        { header = [ h1 [ style [ ( "padding-left", "20px" ) ] ] [ text "RecipeDB" ] ]
        , drawer = []
        , tabs = ( [ text "Home", text "Add Recipe" ], [] )
        , main =
            [ loadPage model ]
        }


loadPage : Model -> Html Msg
loadPage model =
    case model.route of
        Home ->
            (Html.App.map RecipeListMsg (RecipeList.view model.recipes))

        RecipeView _ ->
            (Html.App.map RecipeViewMsg (RecipeView.view model.recipe))

        RecipeEdit _ ->
            (Html.App.map RecipeEditMsg (RecipeEdit.view model.recipe))

        RecipeCreate ->
            (Html.App.map RecipeEditMsg (RecipeEdit.view model.recipe))


showNavigation : Html Msg
showNavigation =
    div [ class "navbar navbar-default" ]
        [ div [ class "container" ]
            [ div [ class "navbarheader" ]
                [ a [ onClick GoHome ]
                    [ p [ class "navbar-brand" ] [ text "RecipeDB" ] ]
                ]
            ]
        ]


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

        GoHome ->
            ( model, Navigation.newUrl (Routing.toHash Routing.Home) )

        Mdl msg' ->
            Material.update msg' model

        SelectTab num ->
            case num of
                0 ->
                    ( model, Navigation.newUrl (Routing.toHash Routing.Home) )

                1 ->
                    ( model, Navigation.newUrl (Routing.toHash Routing.RecipeCreate) )

                _ ->
                    ( model, Navigation.newUrl (Routing.toHash Routing.Home) )


urlUpdate : Result String Page -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    updatePage (Result.withDefault Home result) model


updatePage : Page -> Model -> ( Model, Cmd Msg )
updatePage page model =
    ( { model
        | route = page
        , recipe = newRecipeOnCreate page model.recipe
      }
    , updatePageMessage page
    )


newRecipeOnCreate : Page -> RecipeModel -> RecipeModel
newRecipeOnCreate page model =
    if page == RecipeCreate then
        { model | recipe = (newRecipe) }
    else
        model


updatePageMessage : Page -> Cmd Msg
updatePageMessage page =
    case page of
        Home ->
            Cmd.map RecipeListMsg (RecipeList.fetchAll)

        RecipeView id ->
            Cmd.map RecipeViewMsg (RecipeView.fetchRecipe id)

        RecipeEdit id ->
            Cmd.none

        RecipeCreate ->
            Cmd.none


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format Home (s "")
        , format RecipeEdit (s "recipe" </> s "edit" </> int)
        , format RecipeCreate (s "createrecipe")
        , format RecipeView (s "recipe" </> int)
        ]


hashParser : Navigation.Location -> Result String Page
hashParser location =
    location.hash
        |> String.dropLeft 1
        |> UrlParser.parse identity pageParser


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never
main =
    program
        (Navigation.makeParser hashParser)
        { init = init
        , update = update
        , urlUpdate = urlUpdate
        , view = view
        , subscriptions = subscriptions
        }
