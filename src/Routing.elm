module Routing exposing (..)

import RecipeModel exposing (RecipeId)


type Page
    = Home
    | RecipeView RecipeId
    | RecipeEdit RecipeId
    | RecipeCreate


toHash : Page -> String
toHash page =
    case page of
        Home ->
            "#"

        RecipeView id ->
            "#recipe/" ++ (toString id)

        RecipeEdit id ->
            "#recipe/edit/" ++ (toString id)

        RecipeCreate ->
            "#createrecipe"
