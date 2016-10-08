module RecipeView exposing (..)

import Html exposing (Html, div, p, h2, h5, text, form, input, textarea, button)
import Html.Attributes exposing (class, type', value, name, href)
import Html.Events exposing (onClick, onInput)
import Http
import Task
import Navigation exposing (modifyUrl)
import Routing
import Json.Decode exposing ((:=))
import RecipeModel as RecipeModel exposing (..)
import ErrorHandling exposing (errorToString, showError)


type Msg
    = FetchSuccess Recipe
    | FetchFailure Http.Error
    | DeleteRecipe
    | DeleteFailure Http.Error
    | DeleteSuccess
    | UpdateRecipe


view : RecipeModel -> Html Msg
view model =
    div []
        [ showError model.error
        , showRecipe model.recipe
        ]


showRecipe : Recipe -> Html Msg
showRecipe recipe =
    div [ class "row" ]
        [ div [ class "row" ]
            [ div
                [ class "col-sm-2" ]
                [ button
                    [ href ""
                    , class "btn btn-sm btn-primary"
                    , onClick UpdateRecipe
                    ]
                    [ text "Update recipe" ]
                ]
            , div
                [ class "col-sm-2" ]
                [ button
                    [ href ""
                    , class "btn btn-sm btn-danger"
                    , onClick DeleteRecipe
                    ]
                    [ text "Delete recipe" ]
                ]
            ]
        , div
            [ class "row" ]
            [ div [ class "col-sm-12" ]
                [ div [ class "panel panel-default" ]
                    [ div [ class "panel-heading" ]
                        [ div [ class "panel-title" ]
                            [ text recipe.name ]
                        ]
                    , div [ class "panel-body" ]
                        [ text recipe.description ]
                    ]
                ]
            ]
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
