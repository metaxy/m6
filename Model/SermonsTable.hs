module Model.SermonsTable where

import Import
import Model.Sermons
import Data.Maybe

anyElem :: (Eq x) => [x] -> [x] -> Bool
anyElem x y = Import.any (`elem` x) y

--todo: just use the first

sermonsTable :: [Entity Sermon] -> Widget
sermonsTable sermons' = do
    --filter by language
    lang' <- languages
    let sermons = Import.filter (\x -> anyElem lang' (sermonLanguage $ entityVal x)) sermons'
    
    toWidget $(widgetFile "SermonsTable")


