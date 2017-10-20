module Main exposing (main)

import Dom
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.More
import Ports
import Random
import Records
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
        , input
            [ id "text-input"
            , textInputClassList model.isTextInputOpen
            ] []
        , viewSelectedVideo model.selectedVideo
        ]


viewSelectedVideo : Maybe Records.Video -> Html Msg
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

        VideoSuggestions videoSuggestions ->
            case videoSuggestions.videos of
                RemoteData.NotAsked ->
                    Html.text "Now where did I put those videos?"

                RemoteData.Loading ->
                    Html.text "Just one second, I've got some videos I think you'll like."

                RemoteData.Failure e ->
                    Html.text "Something's gone wrong with my videos darn it!"

                RemoteData.Success videos ->
                    viewVideoThumbnails videos videoSuggestions.text


viewVideoThumbnails videos caption =
    let
        viewThumbnail video =
            img
                [ class "video-thumbnail"
                , src video.thumbnailUrl 
                , onClick <| VideoSelected video
                ]
                [ ]

        images =
            List.map viewThumbnail videos

        captionEl =
            p [ class "thumbnail-caption" ] [ text caption ]
    in
    div [] (List.append images [ captionEl ])


textInputClassList isOpen =
    if isOpen then
        classList
            [ ( "hidden", False )
            ]

    else
        classList
            [ ( "hidden", True )
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
    Decode.map CheckClickLocation Decode.value


-- Model


type alias Model =
    { showSpeechBubble : Bool
    , speechBubbleContent : SpeechBubbleContent
    , speechBubbleChoices : List SpeechBubbleContent
    , isTextInputOpen : Bool
    , selectedVideo : Maybe Records.Video
    }


type SpeechBubbleContent
    = JustWords String
    | VideoSuggestions Records.VideoSuggestions


getRemoteContent speechBubbleContent =
    case speechBubbleContent of
        VideoSuggestions videoSuggestions ->
            case videoSuggestions.videos of
                RemoteData.NotAsked ->
                    let
                        resultToMsg result =
                            VideosResponseReceived videoSuggestions.id <| RemoteData.fromResult result

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


updateVideoSuggestions : Int -> (RemoteData.WebData (List Records.Video)) -> Model -> Model
updateVideoSuggestions id remoteData model =
    let
        hasMatchingId bubbleContent =
            case bubbleContent of
                VideoSuggestions videoSuggestions ->
                    videoSuggestions.id == id

                _ ->
                    False

        suggestions =
            Records.VideoSuggestions id remoteData "How about some scripture videos?"

        changeBubbleContent modelArg =
            if modelArg.speechBubbleContent |> hasMatchingId  then
                { modelArg | speechBubbleContent = VideoSuggestions suggestions }

            else
                modelArg

        changeBubbleChoices modelArg =
            let
                changeChoice bubbleContent =
                    if bubbleContent |> hasMatchingId then
                        VideoSuggestions suggestions

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
            , VideoSuggestions <| Records.VideoSuggestions 1 RemoteData.NotAsked ""
            ]

        initialModel =
            { showSpeechBubble = False
            , speechBubbleContent = initialBubbleContent
            , speechBubbleChoices = initialChoices
            , isTextInputOpen = False
            , selectedVideo = Nothing
            }
    in
    ( initialModel, Cmd.none )


type Msg
    = CheckClickLocation Encode.Value
    | BotClicked BotPart
    | ChangeBubbleContent SpeechBubbleContent
    | VideosResponseReceived Int (RemoteData.WebData (List Records.Video))
    | VideoSelected Records.Video
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
            theReturn
                |> Return.map (\model -> { model | isTextInputOpen = not model.isTextInputOpen })
                |> Return.effect_ focusTheTextInput

        NotKeyboard ->
            saySomethingNew theReturn


focusTheTextInput model =
    if model.isTextInputOpen then
        Dom.focus "text-input"
            |> Task.attempt (always Nevermind)

    else
        Cmd.none


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
        Records.Video
        (Decode.field "video" Decode.string)
        (Decode.field "thumbnail" Decode.string)
