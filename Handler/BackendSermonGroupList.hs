module Handler.BackendSermonGroupList where

import Import

getBackendSermonGroupListR :: Handler Html
getBackendSermonGroupListR =  do  
    items <- runDB $ selectList [] [Desc SermonsGroupName]
    backendDefaultLayout $ do $(widgetFile "BackendSermonGroupList")
