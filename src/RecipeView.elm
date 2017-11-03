module RecipeView exposing (..)

import Html
    exposing
        ( Html
        , div
        , p
        , ul
        , li
        , h2
        , h3
        , h5
        , text
        , form
        , input
        , textarea
        , button
        )
import Html.Attributes exposing (class, value, name, href)
import Http
import Task
import Navigation exposing (modifyUrl)
import Routing
import RecipeModel as RecipeModel exposing (..)
import ErrorHandling exposing (errorToString, showError)
import Material
import Material.Button as Button
import Material.Options as Options
import Material.Options exposing (cs)


type Msg
    = RecipeFetched (Result Http.Error Recipe)
    | DeleteRecipe
    | RecipeDeleted (Result Http.Error ())
    | UpdateRecipe
    | Mdl (Material.Msg Msg)


view : RecipeModel -> Html Msg
view model =
    div []
        [ showError model.error
        , showRecipe model
        ]


showIngredientFor : IngredientFor -> Html Msg
showIngredientFor i =
    li
        []
        [ text (i.name ++ " x" ++ (toString i.amount) ++ " " ++ i.unit) ]


showRecipe : RecipeModel -> Html Msg
showRecipe model =
    div []
        [ h3
            [ class "recipe-title" ]
            [ text model.recipe.name ]
        , h5
            [ class "recipe-author" ]
            [ text ("Author: " ++ model.recipe.username) ]
        , ul
            []
            (List.map showIngredientFor model.recipe.ingredients)
        , p
            []
            [ text model.recipe.description ]
        , Button.render Mdl
            [ 0 ]
            model.mdl
            [ Button.ripple
            , Button.raised
            , Options.onClick UpdateRecipe
            ]
            [ text "Update Recipe" ]
        , Button.render Mdl
            [ 1 ]
            model.mdl
            [ Button.ripple
            , Button.raised
            , Options.onClick DeleteRecipe
            ]
            [ text "Delete Recipe" ]
        ]


update : Msg -> RecipeModel -> ( RecipeModel, Cmd Msg )
update msg model =
    case msg of
        RecipeFetched (Ok recipe) ->
            ( { model | recipe = recipe }, Cmd.none )

        RecipeFetched (Err error) ->
            ( { model | error = (Just (errorToString error)) }, Cmd.none )

        DeleteRecipe ->
            case model.recipe.id of
                Just id ->
                    ( model, deleteRecipe id )

                Nothing ->
                    ( { model | error = Just "No recipe selected" }, Cmd.none )

        RecipeDeleted (Ok _) ->
            ( initialModel, modifyUrl (Routing.toHash Routing.Home) )

        RecipeDeleted (Err e) ->
            ( { model | error = (Just (errorToString e)) }, Cmd.none )

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

        Mdl msg_ ->
            Material.update Mdl msg_ model


fetchRecipe : RecipeId -> Cmd Msg
fetchRecipe id =
    Http.get (recipeUrl id) recipeDecoder
        |> Http.send RecipeFetched


deleteRecipe : RecipeId -> Cmd Msg
deleteRecipe id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = recipeUrl id
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = True
        }
        |> Http.send RecipeDeleted
