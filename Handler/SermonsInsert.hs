{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module Handler.SermonsInsert where

import Import
import Data.Aeson
import GHC.Generics
import Data.Time.Clock
import Control.Monad
import Model.Sermons
import Data.Maybe
import qualified Data.ByteString.Lazy as B

-- curl -v -H "Accept: application/json" -H "Content-Type: application/json" -X POST -d @test.json http://localhost:3000/api/sermons-insert
-- curl -X POST -d @insert.json http://localhost:3000/api/sermons-insert

data InsertItem = InsertItem { 
     title :: Text
    ,lang :: [Text]
    ,scriptures :: [SermonsScripture]
    ,files :: [SermonsFile]
    ,groupName :: Text
    ,speaker :: Text
    ,picture :: Maybe Text
    ,date :: Maybe Text
    ,seriesName :: Maybe Text
    --,notes :: Maybe Html
} deriving Generic

instance FromJSON InsertItem


getSermonsInsertR :: Handler Html
getSermonsInsertR = error "Not yet implemented: getSermonsInsertR"

postSermonsInsertR :: Handler RepPlain
postSermonsInsertR = do
    val <- parseJsonBody_
    -- get speakerid or create new one
    speaker' <- runDB $ getBy $ UniqueSpeakerName $ speaker val
    speakerId <- case speaker' of
        Just sp -> return $ entityKey sp
        Nothing -> runDB $ insert (SermonsSpeaker (speaker val) Nothing Nothing)
        
    -- get groupid or create new one
    group' <- runDB $ getBy $ UniqueGroupName $ groupName val
    groupId <- case group' of
        Just grp -> return $ entityKey grp
        Nothing -> runDB $ insert $ SermonsGroup $ groupName val
        
    series <- runDB $ getBy $ UniqueSeriesName $ fromMaybe "" $ seriesName val
    -- todo: create series with this name
    
    -- insert sermon 
    _ <- runDB $ insert $
        Sermon {
            sermonTitle = title val
            ,sermonLanguage = lang val
            ,sermonPicture = picture val
            ,sermonNotes = Nothing
            ,sermonGroupId = groupId 
            ,sermonSpeakerId = Just speakerId
            ,sermonSpeakerName = Just $ speaker val
            ,sermonSeriesId = fmap entityKey series
            ,sermonDate = date val
            ,sermonFiles = map (B.toStrict . encode) $ files val
            ,sermonScriptures = map (B.toStrict . encode) $ scriptures val
            }
    return (RepPlain (toContent $ title val))
