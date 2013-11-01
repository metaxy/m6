module Handler.BackendSermonGroupList where

import Import

getBackendSermonGroupListR :: Handler Html
getBackendSermonGroupListR =  do  
    items <- runDB $ selectList [] [Desc SermonGroupName]
    backendDefaultLayout $ do $(widgetFile "BackendSermonGroupList")
