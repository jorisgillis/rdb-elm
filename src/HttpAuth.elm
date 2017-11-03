module HttpAuth exposing (get)

import Http
import OAuth
import Json.Decode exposing (Decoder)


get : OAuth.Token -> String -> Decoder a -> Http.Request a
get token url decoder =
    Http.request
        { method = "GET"
        , headers = OAuth.use token []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = True
        }
