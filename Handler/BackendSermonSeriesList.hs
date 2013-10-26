module Handler.BackendSermonSeriesList where

import Import

getBackendSermonSeriesListR :: Handler Html
getBackendSermonSeriesListR =  do  
    list <- runDB $ selectList [] [Desc SermonSeriesName]
    defaultLayout $ do $(widgetFile "BackendSermonSeriesList")
