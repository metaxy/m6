module Handler.BackendSermonSeriesList where

import Import

getBackendSermonSeriesListR :: Handler Html
getBackendSermonSeriesListR =  do  
    items <- runDB $ selectList [] [Desc SermonsSeriesName]
    backendDefaultLayout $ do $(widgetFile "BackendSermonSeriesList")
