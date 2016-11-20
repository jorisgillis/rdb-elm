module RecipeView exposing (..)

import Html exposing (Html, div, p, h2, h3, h5, text, form, input, textarea, button)
import Html.Attributes exposing (class, type', value, name, href)
import Html.Events exposing (onClick, onInput)
import Http
import Task
import Navigation exposing (modifyUrl)
import Routing
import Json.Decode exposing ((:=))
import RecipeModel as RecipeModel exposing (..)
import ErrorHandling exposing (errorToString, showError)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Options exposing (cs)


type Msg
    = FetchSuccess Recipe
    | FetchFailure Http.Error
    | DeleteRecipe
    | DeleteFailure Http.Error
    | DeleteSuccess
    | UpdateRecipe
    | Mdl (Material.Msg Msg)


view : RecipeModel -> Html Msg
view model =
    div []
        [ showError model.error
        , showRecipe model
        ]


showRecipe : RecipeModel -> Html Msg
showRecipe model =
    div []
        [ h3
            [ class "recipe-title" ]
            [ text model.recipe.name ]
        , p
            []
            [ text model.recipe.description ]
        , Button.render Mdl
            [ 0 ]
            model.mdl
            [ Button.ripple
            , Button.raised
            , Button.onClick UpdateRecipe
            ]
            [ text "Update Recipe" ]
        , Button.render Mdl
            [ 1 ]
            model.mdl
            [ Button.ripple
            , Button.raised
            , Button.onClick DeleteRecipe
            ]
            [ text "Delete Recipe" ]
        ]


update : Msg -> RecipeModel -> ( RecipeModel, Cmd Msg )
update msg model =
    case msg of
        FetchSuccess recipe ->
            ( { model | recipe = recipe }, Cmd.none )

        FetchFailure error ->
            ( { model | error = (Just (errorToString error)) }, Cmd.none )

        DeleteRecipe ->
            case model.recipe.id of
                Just id ->
                    ( model, deleteRecipe id )

                Nothing ->
                    ( { model | error = Just "No recipe selected" }, Cmd.none )

        DeleteFailure error ->
            ( { model | error = (Just (errorToString error)) }, Cmd.none )

        DeleteSuccess ->
            ( initialModel, modifyUrl (Routing.toHash Routing.Home) )

        UpdateRecipe ->
            let
                cmd =
                    case model.recipe.id of
                        Just id ->
                            (Navigation.modifyUrl
                                (Routing.toHash
                                    (Routing.RecipeEdit id)
                                )
                            )

                        Nothing ->
                            (Navigation.modifyUrl
                                (Routing.toHash Routing.RecipeCreate)
                            )
            in
                ( model, cmd )

        Mdl msg' ->
            Material.update msg' model


fetchRecipe : RecipeId -> Cmd Msg
fetchRecipe id =
    Http.get recipeDecoder (recipeUrl id)
        |> Task.perform FetchFailure FetchSuccess


deleteRecipe : RecipeId -> Cmd Msg
deleteRecipe id =
    Http.send Http.defaultSettings (deleteRequest id)
        |> Http.fromJson Json.Decode.value
        |> Task.perform DeleteFailure (\_ -> DeleteSuccess)


deleteRequest : RecipeId -> Http.Request
deleteRequest id =
    { verb = "DELETE"
    , headers = []
    , url = recipeUrl id
    , body = Http.empty
    }
