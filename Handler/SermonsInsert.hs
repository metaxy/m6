{-# LANGUAGE DeriveGeneric #-}

module Handler.SermonsInsert where

import Import
import Data.Aeson
import GHC.Generics
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
    itemTitle :: Text, 
    itemAlias :: Text,
    itemLang :: [Text],
    itemGroupNew :: Maybe InsertGroup,
    itemGroup :: Maybe Text,
    itemSpeakerNew :: Maybe InsertSpeaker,
    itemSpeaker :: Maybe Text,
    itemFiles :: [InsertFile],
    itemPicture :: Maybe Text,
    itemCatAlias :: Text,
    itemScriptures :: [InsertScripture]
} deriving Generic


instance FromJSON InsertItem
instance FromJSON InsertFile
instance FromJSON InsertScripture
instance FromJSON InsertSpeaker
instance FromJSON InsertGroup

maybeToEither :: (Maybe a) -> (Maybe Text) -> Either a Text
maybeToEither Nothing (Just a) = Right a
maybeToEither (Just b) Nothing = Left b
maybeToEither (Just b) (Just _) = Left b
maybeToEither Nothing Nothing = Right ""

cScripture :: SermonSermonId -> InsertScripture -> SermonScripture
cScripture sermonId x = SermonScripture (sBook x) (sCap1 x) (sVers1 x) (sCap2 x) (sVers2 x) (sText x) sermonId

cFile :: SermonSermonId -> InsertFile -> SermonFile
cFile sermonId x = SermonFile (fileTitle x) (fileType x) (filePath x) sermonId

getSermonsInsertR :: Handler Html
getSermonsInsertR = error "Not yet implemented: getSermonsInsertR"

postSermonsInsertR :: Handler RepPlain
postSermonsInsertR = do
    val <- parseJsonBody_
    -- get speakerid or create new one
    speakerId <- case (maybeToEither (itemSpeakerNew val) (itemSpeaker val)) of
         Left i -> runDB $ insert (SermonSpeaker (speakerName i) (speakerAlias i) Nothing Nothing)
         Right t -> fmap entityKey $ runDB $ getBy404 $ UniqueSpeakerAlias t
    -- get groupid or create new one
    groupId <- case (maybeToEither (itemGroupNew val) (itemGroup val)) of
         Left i -> runDB $ insert (SermonGroup (groupName i) (groupAlias i))
         Right t -> fmap entityKey $ runDB $ getBy404 $ UniqueGroupAlias t
    -- insert sermon 
    sermonId <- runDB $ insert $
        SermonSermon (itemTitle val) (itemAlias val) (itemLang val) (itemPicture val) Nothing groupId (Just speakerId)
    -- insert scripture references
    _ <- mapM (runDB . insert . (cScripture sermonId)) (itemScriptures val)
    -- insert files
    _ <- mapM (runDB . insert . (cFile sermonId)) (itemFiles val)
    
    let a = itemTitle val
    return (RepPlain (toContent a))
