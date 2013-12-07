module Handler.SermonsList where

import Import
import Yesod.Paginator
import Model.SermonsTable
import Model.Sermons
import qualified Data.Text as T
import Database.Persist.Sql
import Data.Maybe
--filterBySpeaker :: Maybe Text -> [Filter Sermon]
--filterBySpeaker Nothing = []
--filterBySpeaker (Just s) = [SermonSpeakerName ==. Just s]

filterBySpeaker' :: Maybe Text -> Text
filterBySpeaker' Nothing = ""
filterBySpeaker' (Just s) = " AND \"speaker_name\" = ? "
--filterBySpeaker' (Just s) = " AND \"speaker_name\" = \"" `T.append` s `T.append` "\" "

--filterByDate :: Maybe Text -> [Filter Sermon]
--filterByDate Nothing = []
--filterByDate (Just s) = [SermonDate ==. (Just $ fromDisplayDate s)]

filterByDate' :: Maybe Text -> Text
filterByDate' Nothing = ""
filterByDate' (Just s) = " AND \"date\" = ? "

nullNothing :: Maybe Text -> Maybe Text
nullNothing Nothing = Nothing
nullNothing (Just s)
    | T.null s = Nothing
    | otherwise = Just s

getSermonsListR :: SermonsGroupId -> Handler Html
getSermonsListR groupId = do
    
    fS <- lookupGetParam "filter_by_speaker"
    fD <- lookupGetParam "filter_by_date"
    
    grp <- runDB $ get404 $ groupId
  --  let filters =  [SermonGroupId ==. groupId] 
    --                ++ (filterBySpeaker $ nullNothing fS)  ++ (filterByDate $ nullNothing fD)
                    
   -- sermons'' <- runDB $ selectList filters [Asc SermonDate]
    let filterBinds = catMaybes $ [nullNothing fS, fmap (fromDisplayDate) $ nullNothing fD]
    sermons'' <- runDB $ rawSql ("SELECT * FROM \"sermon\" WHERE \"group_id\" = ? " `T.append` (filterBySpeaker' $ nullNothing fS) `T.append` (filterByDate' $ nullNothing fD) `T.append` " ORDER BY \"date\";") 
        ([toPersistValue groupId] ++ (map toPersistValue filterBinds))
        
    (sermons', widget) <- paginate 5 sermons''
    
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


postSermonsListR :: SermonsGroupId -> Handler Html
postSermonsListR = error "Not yet implemented: postSermonsListR"
