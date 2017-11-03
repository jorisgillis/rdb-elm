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
import Json.Encode as Encode
import RecipeModel as RecipeModel exposing (..)
import ErrorHandling exposing (errorToString, showError)
import Material
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Options as Options


type Msg
    = UpdateName String
    | UpdateDescription String
    | UpdateIngredientAmount Int String
    | UpdateIngredientUnit Int String
    | AddIngredient
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


formElementIndex : Int -> Int -> Int -> Int
formElementIndex base element order =
    base + element * 4 + order


labeledTextFieldWithSignal : String -> String -> (Int -> String -> m) -> Int -> List (Textfield.Property m)
labeledTextFieldWithSignal label value signal id =
    [ Textfield.label label
    , Textfield.floatingLabel
    , Textfield.text_
    , Textfield.value value
    , Options.onInput (signal id)
    ]


ingredientForForm : Material.Model -> Int -> Int -> IngredientFor -> Html Msg
ingredientForForm mdl baseNumber ingredientNumber i =
    let
        amountValue =
            (toString i.amount)

        amountTextField =
            labeledTextFieldWithSignal "Amount" amountValue UpdateIngredientAmount i.id

        unitTextField =
            labeledTextFieldWithSignal "Unit" i.unit UpdateIngredientUnit i.id
    in
        div [ class "row" ]
            [ Textfield.render Mdl
                [ (formElementIndex baseNumber ingredientNumber 1) ]
                mdl
                amountTextField
                []
            , Textfield.render Mdl
                [ (formElementIndex baseNumber ingredientNumber 2) ]
                mdl
                unitTextField
                []
            ]


recipeForm : RecipeModel -> Html Msg
recipeForm model =
    let
        nameDescriptionForm =
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
            ]

        ingredientHeader =
            [ p []
                [ strong [] [ text "Ingredients" ] ]
            , Button.render Mdl
                [ 5 ]
                model.mdl
                [ Button.raised
                , Button.ripple
                , Options.onClick AddIngredient
                ]
                [ text "Add Ingredient" ]
            ]

        ingredientForms =
            List.indexedMap (ingredientForForm model.mdl 5) model.recipe.ingredients

        buttons =
            [ div
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
                    [ 4 ]
                    model.mdl
                    [ Button.raised
                    , Button.ripple
                    , Options.onClick PersistRecipe
                    ]
                    [ text "Save" ]
                ]
            ]
    in
        form []
            (nameDescriptionForm
                ++ ingredientHeader
                ++ ingredientForms
                ++ buttons
            )


updateIngredientAmount : Int -> Float -> IngredientFor -> IngredientFor
updateIngredientAmount id a i =
    if i.id == id then
        { i | amount = a }
    else
        i


updateIngredientUnit : Int -> String -> IngredientFor -> IngredientFor
updateIngredientUnit id u i =
    if i.id == id then
        { i | unit = u }
    else
        i


updateIngredientProperty : (IngredientFor -> IngredientFor) -> RecipeModel -> RecipeModel
updateIngredientProperty updater m =
    let
        recipe =
            m.recipe

        ingredients =
            recipe.ingredients
    in
        { m
            | recipe =
                { recipe
                    | ingredients = List.map updater ingredients
                }
        }


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

        UpdateIngredientAmount id amountStr ->
            let
                amount =
                    case (String.toFloat amountStr) of
                        Ok a ->
                            a

                        Err _ ->
                            0
            in
                ( updateIngredientProperty (updateIngredientAmount id amount) model
                , Cmd.none
                )

        UpdateIngredientUnit id unit ->
            ( updateIngredientProperty (updateIngredientUnit id unit) model
            , Cmd.none
            )

        AddIngredient ->
            let
                recipe =
                    model.recipe

                newIngredient =
                    { id = 1
                    , name = ""
                    , description = ""
                    , amount = 0
                    , unit = ""
                    }

                newIngredients =
                    (List.append recipe.ingredients [ newIngredient ])

                newRecipe =
                    { recipe | ingredients = newIngredients }
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


ingredientEncoder : IngredientFor -> Encode.Value
ingredientEncoder i =
    Encode.object
        [ ( "id", Encode.int i.id )
        , ( "name", Encode.string i.name )
        , ( "description", Encode.string i.description )
        , ( "amount", Encode.float i.amount )
        , ( "unit", Encode.string i.unit )
        ]


updateRequestBody : RecipeId -> Recipe -> Encode.Value
updateRequestBody id recipe =
    let
        encodedIngredients =
            Encode.list (List.map ingredientEncoder recipe.ingredients)
    in
        Encode.object
            [ ( "id", Encode.int id )
            , ( "name", Encode.string recipe.name )
            , ( "description", Encode.string recipe.description )
            , ( "ingredients", encodedIngredients )
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


createRequestBody : Recipe -> Encode.Value
createRequestBody recipe =
    let
        encodedIngredients =
            Encode.list (List.map ingredientEncoder recipe.ingredients)
    in
        Encode.object
            [ ( "name", Encode.string recipe.name )
            , ( "description", Encode.string recipe.description )
            , ( "ingredients", encodedIngredients )
            ]
