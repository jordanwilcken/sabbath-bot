module Main exposing (main)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
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


view model =
    node "main"
        [ id "the-main" ]
        [ div [ id "sabbath-bot-bounds" ]
            [ img
                [ id "sabbath-bot"
                , src "sabbath-bot.jpg"
                , on "click" checkClickLocation
                ]
                []
            , div
                [ id "speech-bubble"
                , speechBubbleClassList model
                ]
                (viewBubbleContent model.botState)
            ]
        , input
            [ id "text-input"
            , textInputClassList model.isTextInputOpen
            , on "keydown" (Decode.map keydownToMsg keydownDecoder)
            ]
            []
        , viewSelectedVideo model.selectedVideo
        ]


checkClickLocation =
    Decode.map CheckClickLocation Decode.value


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


viewBubbleContent botState =
    case botState of
        Staring ->
            []

        Talking speechBubbleContent ->
            case speechBubbleContent of
                JustWords theWords ->
                    [ Html.text theWords ]

                PicturePlusWords picturePlusWords ->
                    [ img [ src picturePlusWords.imageSource ] [ ]
                    , p [ ] [ text picturePlusWords.words ]
                    ]

                VideoSuggestions videoSuggestions ->
                    case videoSuggestions.videos of
                        RemoteData.NotAsked ->
                            [ Html.text "Now where did I put those videos?" ]

                        RemoteData.Loading ->
                            [ Html.text "Just one second, I've got some videos I think you'll like." ]

                        RemoteData.Failure e ->
                            [ Html.text "Something's gone wrong with my videos darn it!" ]

                        RemoteData.Success videos ->
                            [ viewVideoThumbnails videos videoSuggestions.text ]


viewVideoThumbnails videos caption =
    let
        viewThumbnail video =
            img
                [ class "video-thumbnail"
                , src video.thumbnailUrl
                , onClick <| VideoSelected video
                ]
                []

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
    case model.botState of
        Talking _ ->
            classList
                [ ( "hidden", False )
                , ( "inline-block", True )
                ]

        Staring ->
            classList
                [ ( "hidden", True )
                , ( "inline-block", False )
                ]



-- Model


type alias Model =
    { botState : BotState
    , dontUnderstandCount : Int
    , speechBubbleChoices : List SpeechBubbleContent
    , isTextInputOpen : Bool
    , selectedVideo : Maybe Records.Video
    }


type BotState
    = Staring
    | Talking SpeechBubbleContent


type SpeechBubbleContent
    = JustWords String
    | VideoSuggestions Records.VideoSuggestions
    | PicturePlusWords Records.PicturePlusWords


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
        
        PicturePlusWords _ ->
            Cmd.none

        JustWords _ ->
            Cmd.none


chooseNewContent : Model -> Cmd Msg
chooseNewContent model =
    let
        someContent =
            case model.botState of
                Talking speechBubbleContent ->
                    speechBubbleContent

                Staring ->
                    JustWords "this is just some content I made because I had to"
    in
    List.More.getSomethingDifferent someContent model.speechBubbleChoices
        |> Random.generate NewContentChosen


updateVideoSuggestions : Int -> RemoteData.WebData (List Records.Video) -> Model -> Model
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
            case modelArg.botState of
                Talking speechBubbleContent ->
                    if speechBubbleContent |> hasMatchingId then
                        { modelArg | botState = Talking <| VideoSuggestions suggestions }
                    else
                        modelArg

                Staring ->
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
        initialChoices =
            [ JustWords "Someday I hope to be animated. Someday..."
            , VideoSuggestions <| Records.VideoSuggestions 1 RemoteData.NotAsked ""
            , JustWords "If you click on my keyboard you can write me a message."
            , JustWords "Do you think kids will like me?"
            , PicturePlusWords <|
                Records.PicturePlusWords
                    "garden-of-eden.jpg"
                    "Checkout out this cool Lego creation! It's from a scripture story. Can you tell which story? Do you think you could make a Lego creation of a scripture story? I think that would be super awesome!"
            ]

        initialModel =
            { botState = Staring
            , dontUnderstandCount = 0
            , speechBubbleChoices = initialChoices
            , isTextInputOpen = False
            , selectedVideo = Nothing
            }
    in
    ( initialModel, Cmd.none )


type Msg
    = CheckClickLocation Encode.Value
    | BotClicked BotPart
    | NewContentChosen SpeechBubbleContent
    | UserSaidSomething String
    | UserStillTyping
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

        NewContentChosen bubbleContent ->
            ( model, Cmd.none )
                |> Return.map
                    (\_ ->
                        { model
                            | botState = Talking bubbleContent
                            , dontUnderstandCount = 0
                        }
                    )
                |> Return.command (getRemoteContent bubbleContent)

        VideosResponseReceived id remoteData ->
            ( model, Cmd.none )
                |> Return.map (updateVideoSuggestions id remoteData)

        VideoSelected video ->
            ( model, Cmd.none )
                |> Return.map (\_ -> { model | selectedVideo = Just video })
                |> Return.command (Ports.scrollIntoView "video-container")

        UserSaidSomething thingSaid ->
            let
                ( theResponse, newCount ) =
                    case comeUpWithResponse thingSaid of
                        Ok response ->
                            ( response, 0 )

                        Err _ ->
                            let
                                newCount =
                                    model.dontUnderstandCount + 1

                                formatted =
                                    formatWords <| newCount

                                response =
                                    JustWords <| formatted "I like you. But I don't understand a word you just said."
                            in
                            ( response, newCount )
            in
            ( model, Cmd.none )
                |> Return.map
                    (\currentModel ->
                        { currentModel
                            | botState = Talking <| theResponse
                            , dontUnderstandCount = newCount
                        }
                    )
                |> Return.command (Ports.clearTextInput { })

        UserStillTyping ->
            ( model, Cmd.none )

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
        |> Return.effect_ chooseNewContent



-- subscriptions


subscriptions model =
    Ports.somethingClicked thingClickedToMsg



-- main


main =
    Html.program
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


keydownToMsg keydown =
    if keydown.keyCode == 13 then
        UserSaidSomething keydown.inputValue
    else
        UserStillTyping


keydownDecoder =
    Decode.map2
        Records.Keydown
        (Decode.field "keyCode" Decode.int)
        (Decode.at [ "target", "value" ] Decode.string)


formatWords count words =
    if count > 1 then
        words ++ " (x" ++ toString count ++ ")"
    else
        words


comeUpWithResponse thingSaid =
    let
        thingSaidContains arg =
            String.contains arg thingSaid
    in
    if thingSaidContains "color" then
        JustWords "Orange. My favorite color is orange."
            |> Result.Ok
    else if thingSaidContains "name" then
        JustWords "My name is Sabbath Bot.  Isn't that cool?"
            |> Result.Ok
    else if thingSaidContains "can't" then
        JustWords "I don't like to think about \"can't\". Makes me feel all grumpy. Can we talk about something else instead?"
            |> Result.Ok
    else
        Result.Err "I got nothing, sorry."
