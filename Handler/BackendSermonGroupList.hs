module Handler.BackendSermonGroupList where

import Import

gteBackendSermonGroupListR :: Handler Html
gteBackendSermonGroupListR =  do  
    list <- runDB $ selectList [] [Desc SermonGroupName]
    defaultLayout $ do $(widgetFile "BackendSermonGroupList")
