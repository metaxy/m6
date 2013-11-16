{-# LANGUAGE DeriveGeneric #-}

module Handler.SermonsInsert where

import Import
import Data.Aeson()
import GHC.Generics
import Data.Time.Clock
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
    ,itemSpeaker :: Text
    ,itemPicture :: Maybe Text
    ,itemUTCTime :: Maybe UTCTime
} deriving Generic


instance FromJSON InsertItem
instance FromJSON InsertFile
instance FromJSON InsertScripture
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
            sermonTitle = (itemTitle val) 
            ,sermonAlias = (itemAlias val) 
            ,sermonLanguage = (itemLang val) 
            ,sermonPicture = Nothing 
            ,sermonNotes = Nothing 
            ,sermonGroupId = groupId 
            ,sermonSpeakerId = (Just speakerId)
            ,sermonSpeakerName = Just (itemSpeaker val)
            ,sermonSeriesId = Nothing
            ,sermonTime = (itemUTCTime val)
            ,sermonFiles = (map (cFile) (itemFiles val))
            ,sermonScriptures = (map (cScripture) (itemScriptures val))
            }
    return (RepPlain (toContent $ itemTitle val))
