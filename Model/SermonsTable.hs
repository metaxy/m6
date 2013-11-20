module Model.SermonsTable where

import Import
import Data.Maybe
import qualified Data.Text as T
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html()
import Text.Blaze.Internal
import Data.Time.Clock

formatScripture' :: SermonsScripture -> Html
formatScripture' (SermonsScripture book c1 v1 c2 v2 Nothing) = 
    toHtml $ (show book) ++ " " ++(show c1) ++ ":" ++(show v1)
formatScripture' (SermonsScripture book c1 v1 c2 v2 (Just t))
    | T.null t = formatScripture' (SermonsScripture book c1 v1 c2 v2 Nothing)
    | otherwise = toHtml t
    
formatScripture :: [SermonsScripture] -> Html
formatScripture x = mapM_ formatScripture' x

formatTime :: Maybe UTCTime -> Html
formatTime Nothing = toHtml $ show ""
formatTime (Just x) = toHtml $ show $ utctDay x

downloadLinks :: [SermonsFile] -> Html
downloadLinks = mapM_ downloadLinks'

downloadLinks' :: SermonsFile -> Html
downloadLinks' (SermonsFile _ "audio" path) = a ! href (textValue path) $ i ! class_ "fa-headphones" $ ""
downloadLinks' (SermonsFile _ "video" path) = a ! href (textValue path) $ i ! class_ "fs-film" $ ""
downloadLinks' (SermonsFile _ "text" path) = a ! href (textValue path)  $ i ! class_ "fa-download" $ ""
downloadLinks' (SermonsFile _ _ path) = a ! href (textValue path)  $ i ! class_  "fa-download" $ ""

anyElem :: (Eq x) => [x] -> [x] -> Bool
anyElem x y = Import.any (`elem` x) y

--todo: just use the first

sermonsTable :: [Entity Sermon] -> Widget
sermonsTable sermons' = do
    --filter by language
    lang' <- languages
    let sermons = Import.filter (\x -> anyElem lang' (sermonLanguage $ entityVal x)) sermons'
    
    toWidget $(widgetFile "SermonsTable")


