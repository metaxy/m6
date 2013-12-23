{-# OPTIONS_GHC -XTypeSynonymInstances -XFlexibleInstances #-}

module Model.SermonsTable where

import Import
import Model.Sermons
import Data.Aeson
import Data.Maybe
import qualified Data.Text as T

--todo: just use the first
anyElem :: (Eq x) => [x] -> [x] -> Bool
anyElem x y = Import.any (`elem` x) y


sermonsTable :: [Entity Sermon] -> Widget
sermonsTable sermons = do
    --filter by language
    lang' <- languages
   -- let sermons = Import.filter (\x -> anyElem lang' (sermonLanguage $ entityVal x)) sermons'
    
    toWidget $(widgetFile "SermonsTable")
