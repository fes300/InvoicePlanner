module Generated.Api exposing(..)

import Time exposing (Posix)
import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder


type alias User  =
   { registration_date: Posix
   , email: Email
   , password: Password
   , uuid_: UserId
   }

jsonDecUser : Json.Decode.Decoder ( User )
jsonDecUser =
   Json.Decode.succeed (\pregistration_date pemail ppassword puuid_ -> {registration_date = pregistration_date, email = pemail, password = ppassword, uuid_ = puuid_})
   |> required "registration_date" (jsonDecPosix)
   |> required "email" (jsonDecEmail)
   |> required "password" (jsonDecPassword)
   |> required "uuid_" (jsonDecUserId)

jsonEncUser : User -> Value
jsonEncUser  val =
   Json.Encode.object
   [ ("registration_date", jsonEncPosix val.registration_date)
   , ("email", jsonEncEmail val.email)
   , ("password", jsonEncPassword val.password)
   , ("uuid_", jsonEncUserId val.uuid_)
   ]



type alias CreateUser  =
   { rawemail: Email
   , rawpassword: Password
   }

jsonDecCreateUser : Json.Decode.Decoder ( CreateUser )
jsonDecCreateUser =
   Json.Decode.succeed (\prawemail prawpassword -> {rawemail = prawemail, rawpassword = prawpassword})
   |> required "rawemail" (jsonDecEmail)
   |> required "rawpassword" (jsonDecPassword)

jsonEncCreateUser : CreateUser -> Value
jsonEncCreateUser  val =
   Json.Encode.object
   [ ("rawemail", jsonEncEmail val.rawemail)
   , ("rawpassword", jsonEncPassword val.rawpassword)
   ]



type alias Password  = String

jsonDecPassword : Json.Decode.Decoder ( Password )
jsonDecPassword =
    Json.Decode.string

jsonEncPassword : Password -> Value
jsonEncPassword  val = Json.Encode.string val



type NoContent  =
    NoContent 

jsonDecNoContent : Json.Decode.Decoder ( NoContent )
jsonDecNoContent = 
    let jsonDecDictNoContent = Dict.fromList [("NoContent", NoContent)]
    in  decodeSumUnaries "NoContent" jsonDecDictNoContent

jsonEncNoContent : NoContent -> Value
jsonEncNoContent  val =
    case val of
        NoContent -> Json.Encode.string "NoContent"



type alias Email  = String

jsonDecEmail : Json.Decode.Decoder ( Email )
jsonDecEmail =
    Json.Decode.string

jsonEncEmail : Email -> Value
jsonEncEmail  val = Json.Encode.string val



type alias UserId  = String

jsonDecUserId : Json.Decode.Decoder ( UserId )
jsonDecUserId =
    Json.Decode.string

jsonEncUserId : UserId -> Value
jsonEncUserId  val = Json.Encode.string val


getMessages : (Result Http.Error  ((List String))  -> msg) -> Cmd msg
getMessages toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8000"
                    [ "messages"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (Json.Decode.string))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getMessagesById : String -> (Result Http.Error  (String)  -> msg) -> Cmd msg
getMessagesById capture_id toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8000"
                    [ "messages"
                    , (capture_id)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg Json.Decode.string
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postMessages : String -> (Result Http.Error  (NoContent)  -> msg) -> Cmd msg
postMessages body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8000"
                    [ "messages"
                    ]
                    params
            , body =
                Http.jsonBody (Json.Encode.string body)
            , expect =
                Http.expectJson toMsg jsonDecNoContent
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getUsers : (Result Http.Error  ((List User))  -> msg) -> Cmd msg
getUsers toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8000"
                    [ "users"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecUser))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getUsersById : UserId -> (Result Http.Error  (User)  -> msg) -> Cmd msg
getUsersById capture_id toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8000"
                    [ "users"
                    , (capture_id |> String.fromInt)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecUser
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postUsers : CreateUser -> (Result Http.Error  (NoContent)  -> msg) -> Cmd msg
postUsers body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8000"
                    [ "users"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncCreateUser body)
            , expect =
                Http.expectJson toMsg jsonDecNoContent
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
