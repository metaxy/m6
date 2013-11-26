{-# OPTIONS_GHC -XTypeSynonymInstances -XFlexibleInstances #-}

module Model.SermonsTable where

import Import
import Model.Sermons
import Model.SimpleSearch

import Data.Maybe
import qualified Data.Text as T

--todo: just use the first
anyElem :: (Eq x) => [x] -> [x] -> Bool
anyElem x y = Import.any (`elem` x) y

type S = Entity Sermon

instance TextSearch Sermon where
    toText s = T.pack $ show "Video"

instance Search Sermon where
   match = keywordMatch

filterByTitle' :: Maybe Text -> [Sermon] -> [Sermon]
filterByTitle' Nothing s = s
filterByTitle' (Just text) s 
    | T.null text = s
    | otherwise = search_ text s

sermonsTable :: [Entity Sermon] -> Widget
sermonsTable sermons' = do
    --filter by language
    lang' <- languages
    let sermons = Import.filter (\x -> anyElem lang' (sermonLanguage $ entityVal x)) sermons'
    
    fT <- lookupGetParam "filter_by_title"
    let sermons2 = filterByTitle' fT (map entityVal sermons)
    
    toWidget $(widgetFile "SermonsTable")


