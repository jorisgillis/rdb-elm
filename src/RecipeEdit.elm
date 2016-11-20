module RecipeEdit exposing (..)

import Http
import Html
    exposing
        ( Html
        , div
        , p
        , h2
        , h5
        , text
        , form
        , input
        , textarea
        , button
        , strong
        )
import Html.Attributes
    exposing
        ( class
        , type'
        , value
        , name
        , href
        , cols
        , rows
        , size
        , placeholder
        , method
        )
import Html.Events exposing (onClick, onInput)
import Task
import Navigation exposing (modifyUrl)
import Routing
import Json.Decode exposing ((:=))
import Json.Encode as JSE
import RecipeModel as RecipeModel exposing (..)
import ErrorHandling exposing (errorToString, showError)
import Material
import Material.Button as Button
import Material.Textfield as Textfield


type Msg
    = UpdateName String
    | UpdateDescription String
    | PersistRecipe
    | Cancel
    | UpdateFailure Http.Error
    | UpdateSuccess
    | CreateFailure Http.Error
    | CreateSuccess RecipeId
    | Mdl (Material.Msg Msg)


view : RecipeModel -> Html Msg
view model =
    div []
        [ showError model.error
        , showForm model
        ]


showForm : RecipeModel -> Html Msg
showForm model =
    div []
        [ h2
            [ class "page-header" ]
            [ text "Edit recipe" ]
        , div
            [ class "panel-body" ]
            [ recipeForm model ]
        ]


recipeForm : RecipeModel -> Html Msg
recipeForm model =
    form []
        [ div
            [ class "row" ]
            [ Textfield.render Mdl
                [ 0 ]
                model.mdl
                [ Textfield.label "Name"
                , Textfield.floatingLabel
                , Textfield.text'
                , Textfield.value model.recipe.name
                , Textfield.onInput UpdateName
                ]
            ]
        , div
            [ class "row" ]
            [ Textfield.render Mdl
                [ 1 ]
                model.mdl
                [ Textfield.label "Description"
                , Textfield.floatingLabel
                , Textfield.textarea
                , Textfield.rows 6
                , Textfield.value model.recipe.description
                , Textfield.onInput UpdateDescription
                ]
            ]
        , div
            [ class "row" ]
            [ Button.render Mdl
                [ 2 ]
                model.mdl
                [ Button.raised
                , Button.ripple
                , Button.onClick Cancel
                ]
                [ text "Cancel" ]
            , Button.render Mdl
                [ 3 ]
                model.mdl
                [ Button.raised
                , Button.ripple
                , Button.onClick PersistRecipe
                ]
                [ text "Save" ]
            ]
          --         [ href ""
          --         , class "btn btn-sm btn-primary align-right"
          --         , onClick PersistRecipe
          --         ]
          --         [ text "Save" ]
          --     ]
          -- , div
          --     [ class "col-sm-2" ]
          --     [ div
          --         [ href ""
          --         , class "btn btn-sm btn-primary"
          --         , onClick Cancel
          --         ]
          --         [ text "Cancel" ]
          --     ]
          -- ]
        ]


update : Msg -> RecipeModel -> ( RecipeModel, Cmd Msg )
update msg model =
    case msg of
        UpdateName name ->
            let
                recipe =
                    model.recipe

                newRecipe =
                    { recipe | name = name }
            in
                ( { model | recipe = newRecipe }, Cmd.none )

        UpdateDescription description ->
            let
                recipe =
                    model.recipe

                newRecipe =
                    { recipe | description = description }
            in
                ( { model | recipe = newRecipe }, Cmd.none )

        PersistRecipe ->
            case model.recipe.id of
                Just id ->
                    ( model, updateRecipe id model.recipe )

                Nothing ->
                    ( model, createRecipe model.recipe )

        Cancel ->
            ( model, toRecipeOrHome model.recipe )

        UpdateFailure error ->
            ( { model | error = (Just (errorToString error)) }, Cmd.none )

        UpdateSuccess ->
            ( { model | error = Nothing }, toRecipeOrHome model.recipe )

        CreateFailure error ->
            ( { model | error = (Just (errorToString error)) }, toHome )

        CreateSuccess id ->
            let
                recipe =
                    model.recipe

                newRecipe =
                    { recipe | id = Just id }
            in
                ( { model | recipe = newRecipe, error = Nothing }, toHome )

        Mdl msg' ->
            Material.update msg' model


toRecipeOrHome recipe =
    case recipe.id of
        Just id ->
            Navigation.newUrl
                (Routing.toHash (Routing.RecipeView id))

        Nothing ->
            Navigation.newUrl
                (Routing.toHash (Routing.Home))


toHome =
    Navigation.newUrl
        (Routing.toHash Routing.Home)


idDecoder : Json.Decode.Decoder Int
idDecoder =
    "id" := Json.Decode.int


updateRecipe : RecipeId -> Recipe -> Cmd Msg
updateRecipe id recipe =
    Http.send Http.defaultSettings (updateRequest id recipe)
        |> Http.fromJson Json.Decode.value
        |> Task.perform UpdateFailure (\_ -> UpdateSuccess)


updateRequest : RecipeId -> Recipe -> Http.Request
updateRequest id recipe =
    { verb = "PUT"
    , headers = [ ( "Content-Type", "application/json" ) ]
    , url = baseRecipeUrl
    , body = Http.string (updateRequestBody id recipe)
    }


updateRequestBody : RecipeId -> Recipe -> String
updateRequestBody id recipe =
    let
        jsonRecipe =
            (JSE.object
                [ ( "id", JSE.int id )
                , ( "name", JSE.string recipe.name )
                , ( "description", JSE.string recipe.description )
                ]
            )
    in
        JSE.encode 0 jsonRecipe


createRecipe : Recipe -> Cmd Msg
createRecipe recipe =
    Http.send Http.defaultSettings (createRequest recipe)
        |> Http.fromJson idDecoder
        |> Task.perform CreateFailure CreateSuccess


createRequest : Recipe -> Http.Request
createRequest recipe =
    { verb = "POST"
    , headers = [ ( "Content-Type", "application/json" ) ]
    , url = baseRecipeUrl
    , body = Http.string (createRequestBody recipe)
    }


createRequestBody : Recipe -> String
createRequestBody recipe =
    let
        jsonRecipe =
            (JSE.object
                [ ( "name", JSE.string recipe.name )
                , ( "description", JSE.string recipe.description )
                ]
            )
    in
        JSE.encode 0 jsonRecipe
