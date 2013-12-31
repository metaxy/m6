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
    id :: Value
    ,title :: Text
    ,speaker :: Text
    ,date :: Text
    ,scripture :: Text
    ,links :: Text
    ,video :: [Text]
    ,audio :: [Text]
} deriving Generic

instance ToJSON SermonsListItem

toItem :: Sermon -> SermonId -> SermonsListItem
toItem x y = SermonsListItem 
    (toJSON y)
    (sermonTitle x) 
    (fromMaybe "" $ sermonSpeakerName x) (Ta.toStrict $ renderHtml $ formatDate $ sermonDate x) 
    (Ta.toStrict $ renderHtml $ formatScripture $ sermonScriptures x)
    (Ta.toStrict $ renderHtml $ downloadLinks $ sermonFiles x)
    (Import.map(sermonsFilePath) $ Import.filter((==) "video" . sermonsFileType) $ decodeList $ sermonFiles x)
    (Import.map(sermonsFilePath) $ Import.filter((==) "audio" . sermonsFileType) $ decodeList $ sermonFiles x)
    
getSermonsJsonListR :: SermonsGroupId -> Handler Value
getSermonsJsonListR groupId = do
    addHeader "Access-Control-Allow-Origin" "*"
    sermons <- runDB $ selectList [SermonGroupId ==. groupId] []
    returnJson $ toJSON $ Import.map (\x -> toItem (entityVal x) (entityKey x)) sermons 

getSermonJsonListR :: SermonId -> Handler Value
getSermonJsonListR sermonId = do
    addHeader "Access-Control-Allow-Origin" "*"
    sermon <- runDB $ get404 $ sermonId
    returnJson $ toJSON $ toItem sermon sermonId

getSermonsListR :: SermonsGroupId -> Handler Html
getSermonsListR groupId = do
    
    grp <- runDB $ get404 $ groupId
    
    sermons' <- runDB $ selectList [SermonGroupId ==. groupId] []
       
    table <- widgetToPageContent $ sermonsTable sermons'
    speakers <- runDB $ selectList [] [Asc SermonsSpeakerName]
    
    
    emptyLayout $ do
        addScript $ StaticR javascripts_jquery_dynatable_js
        addStylesheet $ StaticR stylesheets_jquery_dynatable_css
        $(widgetFile "SermonList")


