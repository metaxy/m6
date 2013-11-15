module Handler.SermonsApi where

import Import
import Data.Aeson
    {--
instance ToJSON SermonsGroup where
     toJSON (SermonGroup name alias) = object ["name" .= name, "alias" .= alias]
     
instance ToJSON SermonsSeries where
     toJSON (SermonSeries name alias _) = object ["name" .= name, "alias" .= alias]
     
instance ToJSON SermonsSpeaker where
     toJSON (SermonSpeaker name alias _ _) = object ["name" .= name, "alias" .= alias]
     
instance ToJSON Category where
     toJSON (Category name alias) = object ["name" .= name, "alias" .= alias] 
     
getSermonsApiR :: String -> Handler Value
getSermonsApiR "listGroup" = do
    items <- runDB $ selectList [] [Desc SermonsGroupName]
    jsonToRepJson $ Import.map toJSON items
    
getSermonsApiR "listSpeaker" = do
    items <- runDB $ selectList [] [Desc SermonsSpeakerName]
    jsonToRepJson $ Import.map toJSON items
    
getSermonsApiR "listSeries" = do
    items <- runDB $ selectList [] [Desc SermonsSeriesName]
    jsonToRepJson $ Import.map toJSON items
    
getSermonsApiR "listCat" = do
    items <- runDB $ selectList [] [Desc CategoryName]
    jsonToRepJson $ Import.map toJSON items
    --}
   
getSermonsApiR :: String -> Handler Value
getSermonsApiR _ = do
    error "not implemented"