module Handler.BackendSermonSpeakerList where

import Import

getBackendSermonSpeakerListR :: Handler Html
getBackendSermonSpeakerListR =  do  
    list <- runDB $ selectList [] [Desc SermonsSpeakerName]
    backendDefaultLayout $ do $(widgetFile "BackendSermonSpeakerList")
