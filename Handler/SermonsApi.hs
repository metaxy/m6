module Handler.SermonsApi where

import Import
import Data.Aeson
    
instance ToJSON SermonsGroup where
     toJSON (SermonsGroup name) = object ["name" .= name]
     
instance ToJSON SermonsSeries where
     toJSON (SermonsSeries name _) = object ["name" .= name]
     
instance ToJSON SermonsSpeaker where
     toJSON (SermonsSpeaker name _ _) = object ["name" .= name]
     
     
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
    
getSermonsApiR _ = do
    error "not implemented"