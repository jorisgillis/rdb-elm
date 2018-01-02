module HttpAuth exposing (get, HttpAuthModel, initModel)

import Http
import OAuth
import Json.Decode exposing (Decoder)


type alias HttpAuthModel =
    { token : OAuth.Token }


initModel : Maybe OAuth.Token -> HttpAuthModel
initModel token =
    case token of
        Just v ->
            { token = v }

        Nothing ->
            { token = OAuth.Bearer "" }


get : HttpAuthModel -> String -> Decoder a -> Http.Request a
get model url decoder =
    Debug.log "Sending authenticated"
        Http.request
        { method = "GET"
        , headers = OAuth.use model.token []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = True
        }
