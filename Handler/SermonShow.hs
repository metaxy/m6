module Handler.SermonShow where

import Import
import Data.Maybe
import Data.List
import Model.Sermons
import Data.Aeson
import qualified Data.ByteString.Lazy as B

getFile' :: Text -> [SermonsFile] -> Maybe SermonsFile
getFile' typ = listToMaybe . filter(\x -> (sermonsFileType x) == typ)

videoPlayer :: SermonsFile -> Widget
videoPlayer file = do
    addScript $ StaticR jwplayer6_jwplayer_js
    toWidget [hamlet|
    <div .row>
        <div .col-lg-6 .col-centered>
            <div #video-player>|]
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
    toWidget [hamlet|
    <div .row>
        <div .col-lg-6 .col-centered>
            <div #audio-player>|]
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
    let files = catMaybes $ map (decode . B.fromStrict) $ sermonFiles sermon
    let videoFile = getFile' "video" files
    let audioFile = getFile' "audio" files
    
    -- todo: make cleaner
    
    series <- case (sermonSeriesId sermon) of
         Nothing -> return Nothing
         Just s -> runDB $ get s
         
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
        
