module Handler.SermonShow where

import Import
import Data.Maybe
import Data.List

getFile' :: Text -> [SermonsFile] -> Maybe SermonsFile
getFile' typ = listToMaybe . filter(\x -> (sermonsFileType x) == typ)

videoPlayer :: SermonsFile -> Widget
videoPlayer file = do
    addScript $ StaticR jwplayer6_jwplayer_js
    toWidget [hamlet|<div #video-player>|]
    toWidget [julius|
        jwplayer('video-player').setup({
            primary: 'html5',
            autostart: false,
            controlbar: 'bottom',
            startparam: 'starttime',
            width: 580,
            height: 320,
            file: #{toJSON $ sermonsFilePath file}
        });|]
audioPlayer :: SermonsFile -> Widget
audioPlayer file = do
    addScript $ StaticR jwplayer6_jwplayer_js
    toWidget [hamlet|<div #audio-player>|]
    toWidget [julius|
        jwplayer('audio-player').setup(
                {
                    primary: 'flash',
                    autostart: false,
                    controlbar: 'bottom',
                    width: 380,
                    height: 180,
                    image: "/static/images/logo.png",
                    file: #{toJSON $ sermonsFilePath file}}
           );|]
getSermonShowR :: SermonId -> Handler Html
getSermonShowR sermonId = do
    sermon <- runDB $ get404 sermonId
    let files = sermonFiles sermon
    let videoFile = getFile' "video" files
    let audioFile = getFile' "audio" files
    
    defaultLayout $ do 
        setTitle $ toHtml $ sermonTitle sermon
        toWidget [hamlet|<h1> #{sermonTitle sermon}|]
        case videoFile of
             Just f -> videoPlayer f
             Nothing -> do
                case audioFile of
                     Just f -> audioPlayer f
                     Nothing -> return ()
        $(widgetFile "SermonShow")
        
