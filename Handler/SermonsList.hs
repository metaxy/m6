{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module Handler.SermonsList where

import Import
import Yesod.Paginator
import Model.SermonsTable
import Model.Sermons
import qualified Data.Text as T
import Database.Persist.Sql
import Data.Maybe
import qualified Data.Text.Lazy as Ta

import Data.Aeson
import GHC.Generics

import Text.Blaze.Html.Renderer.Text
data SermonsListItem = SermonsListItem {
    title :: Text
    ,speaker :: Text
    ,date :: Text
    ,scripture :: Text
    ,links :: Text
} deriving Generic

instance ToJSON SermonsListItem

toItem :: Sermon -> SermonsListItem
toItem x = SermonsListItem (sermonTitle x) 
    (fromMaybe "" $ sermonSpeakerName x) (Ta.toStrict $ renderHtml $ formatDate $ sermonDate x) 
    (Ta.toStrict $ renderHtml $ formatScripture $ sermonScriptures x)
    (Ta.toStrict $ renderHtml $ downloadLinks $ sermonFiles x)
    
getSermonsJsonListR :: SermonsGroupId -> Handler Value
getSermonsJsonListR groupId = do
    setHeader "Access-Control-Allow-Origin" "*"
    grp <- runDB $ get404 $ groupId
    sermons <- runDB $ selectList [SermonGroupId ==. groupId] []
    returnJson $ toJSON $ Import.map (toItem . entityVal) sermons 


postSermonsJsonListR :: SermonsGroupId -> Handler Value
postSermonsJsonListR = error "Not yet implemented: postSermonsListR"

getSermonsListR :: SermonsGroupId -> Handler Html
getSermonsListR groupId = do
    
    grp <- runDB $ get404 $ groupId
    
    sermons' <- runDB $ selectList [SermonGroupId ==. groupId] []
       
    table <- widgetToPageContent $ sermonsTable sermons'
    speakers <- runDB $ selectList [] [Asc SermonsSpeakerName]
    
    
    emptyLayout $ do
    --    addScript $ StaticR javascripts_bootstrap_datepicker_js
        addScript $ StaticR javascripts_jquery_dynatable_js
        
        addStylesheet $ StaticR stylesheets_jquery_dynatable_css
    --    addStylesheet $ StaticR stylesheets_datepicker_css
       {-- toWidget [julius|
            $('.input-group.date').datepicker({
                language: "de",
                forceParse: false,
                calendarWeeks: true,
                autoclose: true,
                todayHighlight: true
            });
            |]--}
          
        $(widgetFile "SermonList")


postSermonsListR :: SermonsGroupId -> Handler Html
postSermonsListR = error "Not yet implemented: postSermonsListR"
