module Handler.BackendSermonGroupList where

import Import

getBackendSermonGroupListR :: Handler Html
getBackendSermonGroupListR =  do  
    items <- runDB $ selectList [] [Desc SermonGroupName]
    defaultLayout $ do $(widgetFile "BackendSermonGroupList")
