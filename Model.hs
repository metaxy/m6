{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}
module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Data.Time
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString
import Data.Maybe
-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
data SermonsScripture = SermonsScripture {
    sermonsScriptureBook :: Int
    ,sermonsScriptureChapter1 :: Int
    ,sermonsScriptureVerse1 :: Int
    ,sermonsScriptureChapter2 :: Int
    ,sermonsScriptureVerse2 :: Int
    ,sermonsScriptureText :: Maybe Text
} deriving (Generic, Eq, Show)

data SermonsFile = SermonsFile {
    sermonsFileTitle :: Maybe Text
    ,sermonsFilePath :: Text
    ,sermonsFileType :: Text
} deriving (Generic, Eq, Show)

decodeList :: FromJSON a => [ByteString] -> [a]
decodeList x = catMaybes $ Prelude.map(decode . LBS.fromStrict) x 

instance FromJSON SermonsScripture 
instance ToJSON SermonsScripture 
instance FromJSON SermonsFile 
instance ToJSON SermonsFile 


share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
