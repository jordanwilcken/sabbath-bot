module Main exposing (main)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.More
import Ports
import Random
import RemoteData exposing (RemoteData)
import Return exposing (Return, map)
import Task
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)


view model =
    node "main" [ id "the-main" ]
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
        , viewSelectedVideo model.selectedVideo
        ]


viewSelectedVideo : Maybe Video -> Html Msg
viewSelectedVideo maybeVideo =
    let
        ( containerClass, containerChildren ) =
            case maybeVideo of
                Just video ->
                    ( "intrinsic-container intrinsic-container-4x3", [ iframe [ src video.url ] [] ] )

                Nothing ->
                    ( "hidden", [] )
    in
    div
        [ id "video-container"
        , class containerClass
        ]
        containerChildren


viewBubbleContent speechBubbleContent =
    case speechBubbleContent of
        JustWords theWords ->
            Html.text theWords

        VideoSuggestions id remoteData ->
            case remoteData of
                RemoteData.NotAsked ->
                    Html.text "Now where did I put those videos?"

                RemoteData.Loading ->
                    Html.text "Just one second, I've got some videos I think you'll like."

                RemoteData.Failure e ->
                    Html.text "Something's gone wrong with my videos darn it!"

                RemoteData.Success videos ->
                    viewVideoThumbnails videos


viewVideoThumbnails videos =
    let
        viewThumbnail video =
            img
                [ src video.thumbnailUrl 
                , onClick <| VideoSelected video
                ]
                [ ]
    in
    div [] <|
        List.map viewThumbnail videos
        

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
    , selectedVideo : Maybe Video
    }


type SpeechBubbleContent
    = JustWords String
    | VideoSuggestions Int (RemoteData.WebData (List Video))


type alias Video =
    { url : String
    , thumbnailUrl : String
    }


getRemoteContent speechBubbleContent =
    case speechBubbleContent of
        VideoSuggestions id remoteData ->
            case remoteData of
                RemoteData.NotAsked ->
                    let
                        resultToMsg result =
                            VideosResponseReceived id <| RemoteData.fromResult result

                        videosRequest =
                            Http.get "content-data/popular-cartoons.json" (Decode.list videoDecoder)
                    in
                    Http.send resultToMsg videosRequest

                _ ->
                    Cmd.none

        JustWords _ ->
            Cmd.none


changeBubbleContent : Model -> Cmd Msg
changeBubbleContent model =
    List.More.getSomethingDifferent model.speechBubbleContent model.speechBubbleChoices
        |> Random.generate ChangeBubbleContent


updateVideoSuggestions : Int -> (RemoteData.WebData (List Video)) -> Model -> Model
updateVideoSuggestions id remoteData model =
    let
        hasMatchingId bubbleContent =
            case bubbleContent of
                VideoSuggestions suggestionsId _ ->
                    suggestionsId == id

                _ ->
                    False

        changeBubbleContent modelArg =
            if modelArg.speechBubbleContent |> hasMatchingId  then
                { modelArg | speechBubbleContent = VideoSuggestions id remoteData }

            else
                modelArg

        changeBubbleChoices modelArg =
            let
                changeChoice bubbleContent =
                    if bubbleContent |> hasMatchingId then
                        VideoSuggestions id remoteData

                    else
                        bubbleContent
            in
            { modelArg | speechBubbleChoices = List.map changeChoice modelArg.speechBubbleChoices }
    in
    model
        |> changeBubbleContent
        |> changeBubbleChoices
            


-- init


init =
    let
        initialBubbleContent =
            JustWords "I'm told I have to 'Say something new'."

        initialChoices =
            [ initialBubbleContent
            , JustWords "..."
            , VideoSuggestions 1 RemoteData.NotAsked
            ]

        initialModel =
            { showSpeechBubble = False
            , speechBubbleContent = initialBubbleContent
            , speechBubbleChoices = initialChoices
            , textInputOpened = False
            , selectedVideo = Nothing
            }
    in
    ( initialModel, Cmd.none )


type Msg
    = CheckClickLocation Encode.Value
    | BotClicked BotPart
    | ChangeBubbleContent SpeechBubbleContent
    | VideosResponseReceived Int (RemoteData.WebData (List Video))
    | VideoSelected Video
    | Nevermind


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

        ChangeBubbleContent bubbleContent ->
            ( model, Cmd.none )
                |> Return.map (\_ -> { model | speechBubbleContent = bubbleContent })
                |> Return.command (getRemoteContent bubbleContent)

        VideosResponseReceived id remoteData ->
            ( model, Cmd.none )
                |> Return.map (updateVideoSuggestions id remoteData)

        VideoSelected video ->
            ( model, Cmd.none )
                |> Return.map (\_ -> { model | selectedVideo = Just video })
                |> Return.command (Ports.scrollIntoView "video-container")

        Nevermind ->
            ( model, Cmd.none )


respondToClick botPart theReturn =
    case botPart of
        Keyboard ->
            Return.map
                (\model -> { model | textInputOpened = not model.textInputOpened })
                theReturn

        NotKeyboard ->
            saySomethingNew theReturn


saySomethingNew : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
saySomethingNew theReturn =
    theReturn
        |> Return.map (\model -> { model | showSpeechBubble = True })
        |> Return.effect_ changeBubbleContent


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


videoDecoder =
    Decode.map2
        Video
        (Decode.field "video" Decode.string)
        (Decode.field "thumbnail" Decode.string)
