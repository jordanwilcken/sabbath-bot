port module Ports exposing (checkClickLocation, somethingClicked)

import Json.Encode


port checkClickLocation : Json.Encode.Value -> Cmd msg

port somethingClicked : (String -> msg) -> Sub msg
