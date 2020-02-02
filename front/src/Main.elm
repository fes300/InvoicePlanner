module Main exposing (..)

import Generated.Api exposing (..)
import Browser
import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)


main =
    Browser.element { init = \() -> init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none }

init : ( Model, Cmd Users )
init =
    ( Model Dict.empty "" Nothing
    , Api.getUsers
    )


type Users = Maybe String


update users model =
    case users of
        Just us -> us
        Nothing -> ""


view model =
    div []
        [ div [] [ text model ] ]
