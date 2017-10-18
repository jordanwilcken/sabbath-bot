port module Ports exposing (checkClickLocation, scrollIntoView, somethingClicked)

import Json.Encode


port checkClickLocation : Json.Encode.Value -> Cmd msg

port scrollIntoView : String -> Cmd msg

port somethingClicked : (String -> msg) -> Sub msg
