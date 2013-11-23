module Handler.SermonsList where

import Import
import Yesod.Paginator
import Model.SermonsTable

getSermonsListR :: Text -> Text -> Handler Html
getSermonsListR cat groupAlias = do
    grp <- runDB $ getBy404 $ UniqueGroupAlias groupAlias
    
    (sermons',widget) <- runDB $ selectPaginated 5 [SermonGroupId ==. entityKey grp] []
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
