module Handler.BackendCatList where

import Import

getBackendCatListR :: Handler Html
getBackendCatListR = do  
    categories <- runDB $ selectList [] [Desc CategoryTitle]
    defaultLayout $ do $(widgetFile "BackendCatList")
