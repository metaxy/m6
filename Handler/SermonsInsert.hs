{-# LANGUAGE DeriveGeneric #-}

module Handler.SermonsInsert where

import Import
import Data.Aeson()
import GHC.Generics
import Data.Time.Clock
import Data.Time.Calendar()
-- curl -v -H "Accept: application/json" -H "Content-Type: application/json" -X POST -d @test.json http://localhost:3000/api/sermons-insert
-- curl -X POST -d @test.json http://localhost:3000/api/sermons-insert
data InsertFile = InsertFile { 
    fileTitle :: Maybe Text, 
    fileType :: Text,
    filePath :: Text
} deriving Generic
data InsertSpeaker = InsertSpeaker { 
    speakerName :: Text, 
    speakerAlias :: Text
} deriving Generic
data InsertGroup = InsertGroup { 
    groupName :: Text, 
    groupAlias :: Text
} deriving Generic

data InsertScripture = InsertScripture { 
    sBook :: Int, 
    sCap1 :: Int,
    sVers1 :: Int,
    sCap2 :: Int,
    sVers2 :: Int,
    sText :: Maybe Text
} deriving Generic

data InsertItem = InsertItem { 
    itemTitle :: Text
    ,itemAlias :: Text
    ,itemLang :: [Text]
    ,itemCatAlias :: Text
    ,itemScriptures :: [InsertScripture]
    ,itemFiles :: [InsertFile]
    ,itemGroupNew :: Maybe InsertGroup
    ,itemGroup :: Maybe Text
    ,itemSpeakerNew :: Maybe InsertSpeaker
    ,itemSpeaker :: Maybe Text
    ,itemPicture :: Maybe Text
    --,itemDay :: Maybe Day
    ,itemUTCTime :: Maybe UTCTime
} deriving Generic


instance FromJSON InsertItem
instance FromJSON InsertFile
instance FromJSON InsertScripture
instance FromJSON InsertSpeaker
instance FromJSON InsertGroup

maybeToEither :: (Maybe a) -> (Maybe Text) -> Either a Text
maybeToEither Nothing (Just a) = Right a
maybeToEither (Just b) Nothing = Left b
maybeToEither (Just b) _ = Left b
maybeToEither Nothing Nothing = Right ""

cScripture :: InsertScripture -> SermonsScripture
cScripture x = SermonsScripture (sBook x) (sCap1 x) (sVers1 x) (sCap2 x) (sVers2 x) (sText x)

cFile :: InsertFile -> SermonsFile
cFile x = SermonsFile (fileTitle x) (fileType x) (filePath x)

getSermonsInsertR :: Handler Html
getSermonsInsertR = error "Not yet implemented: getSermonsInsertR"

postSermonsInsertR :: Handler RepPlain
postSermonsInsertR = do
    val <- parseJsonBody_
    -- get speakerid or create new one
    speakerId <- case (maybeToEither (itemSpeakerNew val) (itemSpeaker val)) of
         Left i -> runDB $ insert (SermonsSpeaker (speakerName i) (speakerAlias i) Nothing Nothing)
         Right t -> fmap entityKey $ runDB $ getBy404 $ UniqueSpeakerAlias t
    -- get groupid or create new one
    groupId <- case (maybeToEither (itemGroupNew val) (itemGroup val)) of
         Left i -> runDB $ insert (SermonsGroup (groupName i) (groupAlias i))
         Right t -> fmap entityKey $ runDB $ getBy404 $ UniqueGroupAlias t
    -- insert sermon 
    sermonId <- runDB $ insert $
        Sermon {
            sermonTitle = (itemTitle val) 
            ,sermonAlias = (itemAlias val) 
            ,sermonLanguage = (itemLang val) 
            ,sermonPicture = Nothing 
            ,sermonNotes = Nothing 
            ,sermonGroupId = groupId 
            ,sermonSpeaker = (Just speakerId)
            ,sermonSpeakerName = Nothing
            ,sermonSeriesId = Nothing
            ,sermonDay = Nothing
            ,sermonTime = (itemUTCTime val)
            ,sermonFiles = (map (cFile) (itemFiles val))
            ,sermonScriptures = (map (cScripture) (itemScriptures val))
            }
    
    let a = itemTitle val
    return (RepPlain (toContent $ show $ entityKey sermonId))
