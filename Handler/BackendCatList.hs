module Handler.BackendCatList where

import Import

getBackendCatListR :: Handler Html
getBackendCatListR = do  
    categories <- runDB $ selectList [] [Desc CategoryName]
    backendDefaultLayout $ do $(widgetFile "BackendCatList")
