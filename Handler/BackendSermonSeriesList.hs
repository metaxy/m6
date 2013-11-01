module Handler.BackendSermonSeriesList where

import Import

getBackendSermonSeriesListR :: Handler Html
getBackendSermonSeriesListR =  do  
    items <- runDB $ selectList [] [Desc SermonSeriesName]
    backendDefaultLayout $ do $(widgetFile "BackendSermonSeriesList")
