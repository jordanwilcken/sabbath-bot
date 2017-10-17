module Main exposing (main)

import Json.Decode as Decode
import Json.Encode as Encode
import List.More
import Ports
import Random
import RemoteData exposing (RemoteData)
import Return exposing (Return, map)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)


view model =
    node "main" []
        [ div [ id "sabbath-bot-bounds"]
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
                [ viewBubbleContent model.speechBubbleContent ]
            ]
        , input [ id "text-input" ] []
        ]


viewBubbleContent speechBubbleContent =
    case speechBubbleContent of
        JustWords text ->
            Html.text text

        VideoSuggestions remoteData ->
            Html.text "It's time to display some videos!"


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
    Decode.map CheckClickLocation Decode.value


-- Model


type alias Model =
    { showSpeechBubble : Bool
    , speechBubbleContent : SpeechBubbleContent
    , speechBubbleChoices : List SpeechBubbleContent
    , textInputOpened : Bool
    }


type SpeechBubbleContent
    = JustWords String
    | VideoSuggestions (RemoteData List Video)


type alias Video =
    { url : String
    , thumbnailUrl : String
    }


saySomethingNew : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
saySomethingNew theReturn =
    theReturn
        |> Return.map (\model -> { model | showSpeechBubble = True })
        |> Return.effect_ changeBubbleContent


changeBubbleContent : Model -> Cmd Msg
changeBubbleContent model =
    List.More.getSomethingDifferent model.speechBubbleContent model.speechBubbleChoices
        |> Random.generate ChangeBubbleContent


-- init


init =
    let
        initialBubbleContent =
            JustWords "I'm told I have to 'Say something new'."

        initialChoices =
            [ initialBubbleContent
            , JustWords "..."
            , VideoSuggestions RemoteData.NotAsked
            ]

        initialModel =
            { showSpeechBubble = False
            , speechBubbleContent = initialBubbleContent
            , speechBubbleChoices = initialChoices
            , textInputOpened = False
            }
    in
    ( initialModel, Cmd.none )


type Msg
    = CheckClickLocation Encode.Value
    | BotClicked BotPart
    | ChangeBubbleContent SpeechBubbleContent


type BotPart
    = Keyboard
    | NotKeyboard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckClickLocation clickEventJson ->
            ( model, Ports.checkClickLocation clickEventJson )

        BotClicked botPart ->
            respondToClick botPart ( model, Cmd.none )

        ChangeBubbleContent newContent ->
            Return.map
                -- cover the case where we don't have the remote data that is the newContent
                (\_ -> { model | speechBubbleContent = newContent })
                ( model, Cmd.none )


respondToClick botPart theReturn =
    case botPart of
        Keyboard ->
            Return.map
                (\model -> { model | textInputOpened = not model.textInputOpened })
                theReturn

        NotKeyboard ->
            saySomethingNew theReturn


-- subscriptions


subscriptions model =
    Ports.somethingClicked thingClickedToMsg
    

-- main


main = Html.program
    { init = init, update = update, view = view, subscriptions = subscriptions }


-- details


thingClickedToMsg thingClickedString =
    if thingClickedString == "keyboard" then
        BotClicked Keyboard

    else
        BotClicked NotKeyboard
