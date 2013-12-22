module Handler.SermonsList where

import Import
import Yesod.Paginator
import Model.SermonsTable
import Model.Sermons
import qualified Data.Text as T
import Database.Persist.Sql
import Data.Maybe


getSermonsListR :: SermonsGroupId -> Handler Html
getSermonsListR groupId = do
    
    grp <- runDB $ get404 $ groupId
    
    sermons' <- runDB $ selectList [SermonGroupId ==. groupId] []
       
    table <- widgetToPageContent $ sermonsTable sermons'
    speakers <- runDB $ selectList [] [Asc SermonsSpeakerName]
    
    
    defaultLayout $ do
        addScript $ StaticR javascripts_bootstrap_datepicker_js
        addScript $ StaticR javascripts_jquery_dynatable_js
        
        addStylesheet $ StaticR stylesheets_jquery_dynatable_css
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
          
        toWidget [julius| 
            $(document).ready( function() {
                $('#sermons-table').dynatable();
             });
         |]
         
        $(widgetFile "SermonList")


postSermonsListR :: SermonsGroupId -> Handler Html
postSermonsListR = error "Not yet implemented: postSermonsListR"
