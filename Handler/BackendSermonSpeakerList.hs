module Handler.BackendSermonSpeakerList where

import Import

getBackendSermonSpeakerListR :: Handler Html
getBackendSermonSpeakerListR =  do  
    list <- runDB $ selectList [] [Desc SermonSpeakerName]
    backendDefaultLayout $ do $(widgetFile "BackendSermonSpeakerList")
