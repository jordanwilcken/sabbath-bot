module Records exposing (Keydown, PicturePlusWords, Video, VideoSuggestions)

import RemoteData


type alias VideoSuggestions =
    { id : Int
    , videos : RemoteData.WebData (List Video)
    , text : String
    }


type alias Video =
    { url : String
    , thumbnailUrl : String
    }


type alias Keydown =
    { keyCode : Int
    , inputValue : String
    }


type alias PicturePlusWords =
    { imageSource : String
    , words : String
    }
