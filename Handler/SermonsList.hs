module Handler.SermonsList where

import Import
import Yesod.Paginator
import Model.SermonsTable
import Model.Sermons
import qualified Data.Text as T

filterBySpeaker :: Maybe Text -> [Filter Sermon]
filterBySpeaker Nothing = []
filterBySpeaker (Just s) 
    | T.null s = []
    | otherwise = [SermonSpeakerName ==. Just s]

filterByDate :: Maybe Text -> [Filter Sermon]
filterByDate Nothing = []
filterByDate (Just s) 
    | T.null s = []
    | otherwise = [SermonDate ==. (Just $ fromDisplayDate s)]
    
getSermonsListR :: Text -> Text -> Handler Html
getSermonsListR cat groupAlias = do
    
    fS <- lookupGetParam "filter_by_speaker"
    fD <- lookupGetParam "filter_by_date"
    
    grp <- runDB $ getBy404 $ UniqueGroupAlias groupAlias
    let filters =  [SermonGroupId ==. entityKey grp] 
                    ++ (filterBySpeaker fS)  ++ (filterByDate fD)
    (sermons',widget) <- runDB $ selectPaginated 25 filters [Asc SermonDate]
    
    table <- widgetToPageContent $ sermonsTable sermons'
    speakers <- runDB $ selectList [] [Asc SermonsSpeakerName]
    
    defaultLayout $ do
        addScript $ StaticR javascripts_bootstrap_datepicker_js
        addStylesheet $ StaticR stylesheets_datepicker_css
        toWidget [julius|
            $('.input-group.date').datepicker({
                language: "de",
                forceParse: false,
                calendarWeeks: true,
                autoclose: true,
                todayHighlight: true
            });
          |]
        $(widgetFile "SermonList")


postSermonsListR :: Text -> Text -> Handler Html
postSermonsListR = error "Not yet implemented: postSermonsListR"
