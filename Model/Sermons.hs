module Model.Sermons where

import Import
import qualified Data.Text as T
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html()
import Text.Blaze.Internal
import Data.Time.Clock
import Data.Time.Calendar

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
formatTime (Just x) = toHtml $ f $ toGregorian $ utctDay x
    where
        f (a,b,c) = (show c) ++ "." ++ show(b) ++ "." ++ (show a)
       
downloadLinks :: [SermonsFile] -> Html
downloadLinks = mapM_ downloadLinks'

downloadLinks' :: SermonsFile -> Html
downloadLinks' (SermonsFile _ "audio" path) = a ! href (textValue path) $ i ! class_ "fa fa-headphones" $ ""
downloadLinks' (SermonsFile _ "video" path) = a ! href (textValue path) $ i ! class_ "fa fs-film" $ ""
downloadLinks' (SermonsFile _ "text" path) = a ! href (textValue path)  $ i ! class_ "fa fa-download" $ ""
downloadLinks' (SermonsFile _ _ path) = a ! href (textValue path)  $ i ! class_  "fa fa-download" $ ""

