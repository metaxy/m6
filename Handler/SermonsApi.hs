module Handler.SermonsApi where

import Import
import Data.Aeson
    
instance ToJSON SermonGroup where
     toJSON (SermonGroup name alias catId) = object ["name" .= name, "alias" .= alias, "catID" .= catId]
     
instance ToJSON SermonSeries where
     toJSON (SermonSeries name alias _) = object ["name" .= name, "alias" .= alias]
     
instance ToJSON SermonSpeaker where
     toJSON (SermonSpeaker name alias _ _) = object ["name" .= name, "alias" .= alias]
     
instance ToJSON Category where
     toJSON (Category name alias) = object ["name" .= name, "alias" .= alias] 
     
getSermonsApiR :: String -> Handler Value
getSermonsApiR "listGroup" = do
    items <- runDB $ selectList [] [Desc SermonGroupName]
    jsonToRepJson $ Import.map toJSON items
    
getSermonsApiR "listSpeaker" = do
    items <- runDB $ selectList [] [Desc SermonSpeakerName]
    jsonToRepJson $ Import.map toJSON items
    
getSermonsApiR "listSeries" = do
    items <- runDB $ selectList [] [Desc SermonSeriesName]
    jsonToRepJson $ Import.map toJSON items
    
getSermonsApiR "listCat" = do
    items <- runDB $ selectList [] [Desc CategoryName]
    jsonToRepJson $ Import.map toJSON items
    
getSermonsApiR _ = do
    error "not implemented"