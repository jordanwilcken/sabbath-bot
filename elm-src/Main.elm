module Main exposing (main)

import Json.Decode as Decode
import Json.Encode as Encode
import Ports
import Return exposing (Return, map)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)


view model =
    div [ id "sabbath-bot-bounds"]
        [ img
            [ id "sabbath-bot"
            , src "sabbath-bot.jpg"
            , on "click" clickEventDecoder
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
        classList
            [ ( "hidden", False )
            , ( "inline-block", True )
            ]

    else
        classList
            [ ( "hidden", True )
            , ( "inline-block", False )
            ]


clickEventDecoder =
    Decode.map CheckClickLocation


-- Model


type alias Model =
    { showSpeechBubble : Bool
    , speechBubbleContent : String
    , textInputOpened : Bool
    }


saySomethingNew model =
    { model
    | showSpeechBubble = True
    , speechBubbleContent = "I am told that I must 'Say something new.'"
    }


-- init


init =
    ( Model False "" False, Cmd.none )


type Msg
    = SaySomethingNew
    | CheckClickLocation Encode.Value
    | BotClicked BotPart


type BotPart
    = Keyboard
    | NotKeyboard


update msg model =
    case msg of
        SaySomethingNew ->
            Return.map saySomethingNew ( model, Cmd.none )

        CheckClickLocation clickEventJson ->
            ( model, Ports.checkClickLocation clickEventJson )

        BotClicked botPart ->
            Return.map (respondToClick botPart) ( model, Cmd.none )


respondToClick botPart model =
    case botPart of
        Keyboard ->
            { model | textInputOpened = not model.textInputOpened }

        NotKeyboard ->
            saySomethingNew model
    

-- main


main = Html.program
    { init = init, update = update, view = view, subscriptions = Ports.somethingClicked stringToBotPart }


stringToBotPart theString =
    if theString == "keyboard" then
        Keyboard

    else
        NotKeyboard
