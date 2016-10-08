module RecipeList exposing (..)

import Http
import Json.Decode as Decode exposing ((:=))
import Task
import Html exposing (..)
import Html.Attributes exposing (class, id, href)
import Html.Events exposing (onClick)
import Navigation
import RecipeModel exposing (RecipeId, Recipe, recipeDecoder)
import Routing
import ErrorHandling exposing (errorToString, showError)


type Msg
    = FetchSuccess { recipes : List Recipe }
    | FetchFailure Http.Error
    | ToRecipe RecipeId
    | CreateRecipe


type alias Model =
    { recipes : List Recipe
    , error : Maybe String
    }


initialModel : Model
initialModel =
    Model [] Nothing


view : Model -> Html Msg
view model =
    div []
        [ showError model.error
        , showCrud
        , div [ class "row" ] [ showRecipes model.recipes ]
        ]


showCrud : Html Msg
showCrud =
    div [ class "row crud" ]
        [ div [ class "col-sm-2" ]
            [ div
                [ href ""
                , class "btn btn-sm btn-success"
                , onClick CreateRecipe
                ]
                [ text "Add recipe" ]
            ]
        ]


showRecipes : List Recipe -> Html Msg
showRecipes recipes =
    div [ class "grid" ] (List.map recipeRow recipes)


recipeRow : Recipe -> Html Msg
recipeRow recipe =
    case recipe.id of
        Just id ->
            div [ class "grid-item" ]
                [ div [ class "panel panel-default" ]
                    [ div [ class "panel-heading" ]
                        [ p [ class "panel-title", onClick (ToRecipe id) ]
                            [ text recipe.name ]
                        ]
                    , div [ class "panel-body" ] [ text recipe.description ]
                    ]
                ]

        Nothing ->
            div [] []


update : Msg -> Model -> ( Model, Cmd Msg )
update message recipes =
    case message of
        FetchSuccess newRecipes ->
            ( Model newRecipes.recipes (Nothing), Cmd.none )

        FetchFailure error ->
            ( Model [] (Just (errorToString error)), Cmd.none )

        ToRecipe id ->
            Debug.log "TORECIPE"
                ( recipes
                , Navigation.newUrl
                    (Routing.toHash (Routing.RecipeView id))
                )

        CreateRecipe ->
            Debug.log "CREATE"
                ( recipes
                , Navigation.newUrl (Routing.toHash Routing.RecipeCreate)
                )


fetchAll : Cmd Msg
fetchAll =
    Http.get recipesDecoder "http://localhost:3000/recipe/"
        |> Task.perform FetchFailure FetchSuccess


type alias Recipes =
    { recipes : List Recipe }


recipesDecoder : Decode.Decoder Recipes
recipesDecoder =
    Decode.object1 Recipes ("recipes" := recipeListDecoder)


recipeListDecoder : Decode.Decoder (List Recipe)
recipeListDecoder =
    Decode.list recipeDecoder
