{-# LANGUAGE DeriveGeneric #-}

module Handler.SermonsInsert where

import Import
import Data.Aeson
import Data.Aeson.TH
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
    newGroup :: Maybe InsertGroup,
    newSpeaker :: Maybe InsertSpeaker,
    itemFiles :: [InsertFile],
    picture :: Maybe Text,
    itemCatAlias :: Text,
    itemGroup :: Maybe Text,
    itemSpeaker :: Maybe Text
} deriving Generic


instance FromJSON InsertItem
instance FromJSON InsertFile
instance FromJSON InsertScripture
instance FromJSON InsertSpeaker
instance FromJSON InsertGroup


getSermonsInsertR :: Handler Html
getSermonsInsertR = error "Not yet implemented: getSermonsInsertR"

postSermonsInsertR :: Handler RepPlain
postSermonsInsertR = do
    val <- parseJsonBody_ 
    -- get speaker
    speakerId <- case (itemSpeaker val) of
        Just speaker -> runDB $ getBy404 $ UniqueSpeakerAlias speaker
        Nothing -> runDB $ getBy404 $ UniqueSpeakerAlias ""
    
    let newSpeakerID = fmap (\x -> runDB $ insert (SermonSpeaker (speakerName x) (speakerAlias x) Nothing Nothing)) (newSpeaker val)
    
    let a = itemTitle val
    return (RepPlain (toContent a))
