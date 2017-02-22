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
        , value
        , name
        , href
        , cols
        , rows
        , size
        , placeholder
        , method
        )
import Navigation exposing (modifyUrl)
import Routing
import Json.Decode exposing (Decoder, int)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode as JSE
import RecipeModel as RecipeModel exposing (..)
import ErrorHandling exposing (errorToString, showError)
import Material
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Options as Options


type Msg
    = UpdateName String
    | UpdateDescription String
    | PersistRecipe
    | Cancel
    | RecipeUpdated (Result Http.Error ())
    | RecipeCreated (Result Http.Error CreateId)
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
                , Textfield.text_
                , Textfield.value model.recipe.name
                , Options.onInput UpdateName
                ]
                []
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
                , Options.onInput UpdateDescription
                ]
                []
            ]
        , div
            [ class "row" ]
            [ Button.render Mdl
                [ 2 ]
                model.mdl
                [ Button.raised
                , Button.ripple
                , Options.onClick Cancel
                ]
                [ text "Cancel" ]
            , Button.render Mdl
                [ 3 ]
                model.mdl
                [ Button.raised
                , Button.ripple
                , Options.onClick PersistRecipe
                ]
                [ text "Save" ]
            ]
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

        RecipeUpdated (Ok _) ->
            ( { model | error = Nothing }, toRecipeOrHome model.recipe )

        RecipeUpdated (Err error) ->
            ( { model | error = (Just (errorToString error)) }, Cmd.none )

        RecipeCreated (Ok id) ->
            let
                recipe =
                    model.recipe

                newRecipe =
                    { recipe | id = Just id.id }
            in
                ( { model | recipe = newRecipe, error = Nothing }, toHome )

        RecipeCreated (Err error) ->
            ( { model | error = (Just (errorToString error)) }, toHome )

        Mdl msg_ ->
            Material.update Mdl msg_ model


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


updateRecipe : RecipeId -> Recipe -> Cmd Msg
updateRecipe id recipe =
    Http.request
        { method = "PUT"
        , headers = []
        , url = baseRecipeUrl
        , body = Http.jsonBody (updateRequestBody id recipe)
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = True
        }
        |> Http.send RecipeUpdated


updateRequestBody : RecipeId -> Recipe -> JSE.Value
updateRequestBody id recipe =
    JSE.object
        [ ( "id", JSE.int id )
        , ( "name", JSE.string recipe.name )
        , ( "description", JSE.string recipe.description )
        ]


type alias CreateId =
    { id : Int }


idDecoder : Decoder CreateId
idDecoder =
    decode CreateId
        |> required "id" int


createRecipe : Recipe -> Cmd Msg
createRecipe recipe =
    Http.post
        baseRecipeUrl
        (Http.jsonBody (createRequestBody recipe))
        idDecoder
        |> Http.send RecipeCreated


createRequestBody : Recipe -> JSE.Value
createRequestBody recipe =
    JSE.object
        [ ( "name", JSE.string recipe.name )
        , ( "description", JSE.string recipe.description )
        ]
