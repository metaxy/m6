module Handler.SermonsList where

import Import
import Yesod.Paginator
import Model.SermonsTable
import Model.Sermons
import qualified Data.Text as T
import Database.Persist.Sql
import Data.Maybe

filterBySpeaker :: Maybe Text -> Text
filterBySpeaker Nothing = ""
filterBySpeaker (Just s) = " AND \"speaker_name\" = ? "

filterByDate :: Maybe Text -> Text
filterByDate Nothing = ""
filterByDate (Just s) = " AND \"date\" = ? "

filterByTitle :: Maybe Text -> Text
filterByTitle Nothing = ""
filterByTitle (Just s) = " AND \"title\" MATCH ? "

nullNothing :: Maybe Text -> Maybe Text
nullNothing Nothing = Nothing
nullNothing (Just s)
    | T.null s = Nothing
    | otherwise = Just s

getSermonsListR :: SermonsGroupId -> Handler Html
getSermonsListR groupId = do
    
    fS' <- lookupGetParam "s"
    fD' <- lookupGetParam "d"
    fT' <- lookupGetParam "t"
    let fS = nullNothing $ fS'
    let fD = nullNothing $ fD'
    let fT = nullNothing $ fT'
   
    grp <- runDB $ get404 $ groupId
    let filterBinds = catMaybes $ [fS, fmap (fromDisplayDate) fD, fT]
   
    sermons2 <- runDB $ rawSql ("SELECT * FROM \"sermon\" WHERE \"group_id\" = ? " `T.append` (filterBySpeaker fS) `T.append` (filterByDate fD) `T.append` (filterByTitle fT) `T.append` " ORDER BY \"date\";") ([toPersistValue groupId] ++ (map toPersistValue filterBinds))
        
    (sermons', widget) <- paginate 25 sermons2
    
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
