module Main exposing (main)

import Return exposing (Return, map)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


view model =
    div [ id "sabbath-bot-bounds"]
        [ img
            [ src "sabbath-bot.jpg"
            , onClick SaySomethingNew
            ]
            []
        , div
            [ id "speech-bubble"
            , speechBubbleClassList model
            ]
            [ text model.speechBubbleContent ]
        ]


speechBubbleClassList model =
    if model.showSpeechBubble then
        classList [ ( "hidden", False ) ]

    else
        classList [ ( "hidden", True ) ]


-- Model


type alias Model =
    { showSpeechBubble : Bool
    , speechBubbleContent : String
    }


saySomethingNew model =
    { model
    | showSpeechBubble = True
    , speechBubbleContent = "I am told that I must 'Say something new.'"
    }


-- init


init =
    ( Model False "", Cmd.none )


type Msg =
    SaySomethingNew


update msg model =
    case msg of
        SaySomethingNew ->
            Return.map saySomethingNew ( model, Cmd.none )


-- main


main = Html.program
    { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
