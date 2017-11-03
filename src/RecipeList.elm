module RecipeList exposing (..)

import Http
import HttpAuth
import Json.Decode exposing (Decoder, list, dict)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Html exposing (..)
import Html.Attributes exposing (class, id, href)
import Navigation
import Material
import Material.Button as Button
import Material.Options as Options
import Material.Card as Card
import Material.Options exposing (css)
import Material.Color as Color
import RecipeModel exposing (RecipeId, Recipe, recipeDecoder)
import Routing
import ErrorHandling exposing (errorToString, showError)


type Msg
    = Fetched (Result Http.Error Recipes)
    | ToRecipe RecipeId
    | CreateRecipe
    | Mdl (Material.Msg Msg)


type alias Recipes =
    { recipes : List Recipe }


type alias Model =
    { recipes : List Recipe
    , error : Maybe String
    , mdl : Material.Model
    }


initialModel : Model
initialModel =
    Model [] Nothing Material.model


view : Model -> Html Msg
view model =
    div []
        [ showError model.error
        , div [ class "row" ] [ showRecipes model ]
        ]


showRecipes : Model -> Html Msg
showRecipes model =
    div [] (List.map (recipeBox model) model.recipes)


recipeBox : Model -> Recipe -> Html Msg
recipeBox model recipe =
    case recipe.id of
        Just id ->
            div [ class "grid-item" ]
                [ Card.view
                    [ css "width" "350px" ]
                    [ Card.title
                        [ Color.text Color.white ]
                        [ Card.head [] [ text recipe.name ] ]
                    , Card.text
                        [ Card.border ]
                        [ text recipe.description ]
                    , Card.actions
                        [ Card.border ]
                        [ Button.render Mdl
                            [ 0, id ]
                            model.mdl
                            [ Button.ripple
                            , Button.raised
                            , Button.colored
                            , Options.onClick (ToRecipe id)
                            ]
                            [ text "View" ]
                        , Button.render Mdl
                            [ 1, id ]
                            model.mdl
                            [ Button.ripple
                            , Button.raised
                            , Button.colored
                            ]
                            [ text "Ingredients" ]
                        ]
                    ]
                ]

        Nothing ->
            div [] []


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Fetched (Ok newRecipes) ->
            ( Model newRecipes.recipes (Nothing) Material.model, Cmd.none )

        Fetched (Err error) ->
            ( Model [] (Just (errorToString error)) Material.model
            , Navigation.newUrl "http://localhost:3000/login"
            )

        ToRecipe id ->
            ( model
            , Navigation.newUrl
                (Routing.toHash (Routing.RecipeView id))
            )

        CreateRecipe ->
            ( model
            , Navigation.newUrl (Routing.toHash Routing.RecipeCreate)
            )

        Mdl msg_ ->
            Material.update Mdl msg_ model


fetchAll : Cmd Msg
fetchAll =
    HttpAuth.get "http://localhost:3000/recipe/" recipesDecoder
        |> Http.send Fetched


recipesDecoder : Decoder Recipes
recipesDecoder =
    decode Recipes
        |> required "recipes" (list recipeDecoder)
