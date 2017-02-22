module RecipeModel exposing (..)

import Material
import Json.Decode exposing (int, string, float, nullable, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


type alias RecipeId =
    Int


type alias Recipe =
    { id : Maybe RecipeId
    , name : String
    , description : String
    }


newRecipe : Recipe
newRecipe =
    { id = Nothing
    , name = ""
    , description = ""
    }


recipeDecoder : Decoder Recipe
recipeDecoder =
    decode Recipe
        |> required "id" (nullable int)
        |> required "name" string
        |> required "description" string


type alias RecipeModel =
    { recipe : Recipe
    , error : Maybe String
    , mdl : Material.Model
    }


initialModel : RecipeModel
initialModel =
    RecipeModel newRecipe Nothing Material.model


createNewModel : RecipeModel
createNewModel =
    RecipeModel newRecipe Nothing Material.model


baseRecipeUrl : String
baseRecipeUrl =
    "http://localhost:3000/recipe/"


recipeUrl : RecipeId -> String
recipeUrl id =
    baseRecipeUrl ++ (toString id)
