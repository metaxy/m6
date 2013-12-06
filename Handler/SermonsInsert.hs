{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module Handler.SermonsInsert where

import Import
import Data.Aeson
import GHC.Generics
import Data.Time.Clock
import Control.Monad
import Model.Sermons

-- curl -v -H "Accept: application/json" -H "Content-Type: application/json" -X POST -d @test.json http://localhost:3000/api/sermons-insert
-- curl -X POST -d @test.json http://localhost:3000/api/sermons-insert
data InsertSpeaker = InsertSpeaker { 
     speakerName :: Text
    ,speakerAlias :: Text
} deriving Generic
data InsertGroup = InsertGroup { 
     groupName :: Text
    ,groupAlias :: Text
} deriving Generic


data InsertItem = InsertItem { 
     itemTitle :: Text
    ,itemAlias :: Text
    ,itemLang :: [Text]
    ,itemCatAlias :: Text
    ,itemScriptures :: [SermonsScripture]
    ,itemFiles :: [SermonsFile]
    ,itemGroupNew :: Maybe InsertGroup
    ,itemGroup :: Maybe Text
    ,itemSpeaker :: Text
    ,itemPicture :: Maybe Text
    ,itemTime :: Maybe Text
} deriving Generic


instance FromJSON InsertItem
{--
instance FromJSON SermonsScripture where
    parseJSON (Object v) = SermonsScripture   <$>
                          v .: "book" <*>
                          v .: "cap1" <*>
                          v .: "vers1" <*>
                          v .: "cap2" <*>
                          v .: "vers2" <*>
                          v .: "text"
    parseJSON _ = mzero
    --}
instance FromJSON InsertGroup

maybeToEither :: (Maybe a) -> (Maybe Text) -> Either a Text
maybeToEither Nothing (Just a) = Right a
maybeToEither (Just b) Nothing = Left b
maybeToEither (Just b) _ = Left b
maybeToEither Nothing Nothing = Right ""


getSermonsInsertR :: Handler Html
getSermonsInsertR = error "Not yet implemented: getSermonsInsertR"

postSermonsInsertR :: Handler RepPlain
postSermonsInsertR = do
    val <- parseJsonBody_
    -- get speakerid or create new one
    speaker <- runDB $ getBy $ UniqueSpeakerName (itemSpeaker val)
    speakerId <- case speaker of
        Just sp -> return $ entityKey sp
        Nothing -> runDB $ insert (SermonsSpeaker (itemSpeaker val) Nothing Nothing)
        
    -- get groupid or create new one
    groupId <- case (maybeToEither (itemGroupNew val) (itemGroup val)) of
         Left i -> runDB $ insert (SermonsGroup (groupName i) (groupAlias i))
         Right t -> fmap entityKey $ runDB $ getBy404 $ UniqueGroupAlias t
    -- insert sermon 
    _ <- runDB $ insert $
        Sermon {
            sermonTitle = itemTitle val
            ,sermonAlias = itemAlias val
            ,sermonLanguage = itemLang val
            ,sermonPicture = Nothing 
            ,sermonNotes = Nothing 
            ,sermonGroupId = groupId 
            ,sermonSpeakerId = Just speakerId
            ,sermonSpeakerName = Just $ itemSpeaker val
            ,sermonSeriesId = Nothing
            ,sermonTime = itemTime val
            ,sermonFiles = lazyToStrictBS $ encode $ itemFiles val
            ,sermonScriptures = lazyToStrictBS $ encode $ itemScriptures val
            }
    return (RepPlain (toContent $ itemTitle val))
