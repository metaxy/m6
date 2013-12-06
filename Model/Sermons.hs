module Model.Sermons where

import Import
import qualified Data.Text as T
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html()
import Text.Blaze.Internal
import qualified Data.ByteString      as BS
import Data.Aeson
import Data.Maybe
import Data.Time.Format
import Data.Time
import System.Locale
--import Data.Time.Clock
--import Data.Time.Calendar
--fromDisplayDate :: Text -> Day
fromDisplayDate x = T.pack $ formatTime defaultTimeLocale "%F" day
    where
        day = readTime defaultTimeLocale "%d.%m.%Y" (T.unpack x) :: Day
        
--from interal to display
--2013-05-12 -> 12.05.2013
--toDisplayDate :: Text -> Text
toDisplayDate x = T.pack $ formatTime defaultTimeLocale "%d.%m.%Y" day
    where
        day = readTime defaultTimeLocale "%F" (T.unpack x) :: Day
        
formatScripture' :: SermonsScripture -> Html
formatScripture' (SermonsScripture book c1 v1 c2 v2 Nothing) = 
    toHtml $ (show book) ++ " " ++(show c1) ++ ":" ++(show v1)
formatScripture' (SermonsScripture book c1 v1 c2 v2 (Just t))
    | T.null t = formatScripture' (SermonsScripture book c1 v1 c2 v2 Nothing)
    | otherwise = toHtml t
 
formatScripture :: [BS.ByteString] -> Html
formatScripture x = mapM_ formatScripture' $ decodeList x 

formatDate :: Maybe Text -> Html
formatDate Nothing = toHtml $ show ""
formatDate (Just t) = toHtml $ toDisplayDate t
--formatTime (Just x) = toHtml $ f $ toGregorian $ utctDay x
  --  where
--        f (a,b,c) = (show c) ++ "." ++ show(b) ++ "." ++ (show a)
       
downloadLinks :: [BS.ByteString] -> Html
downloadLinks x = mapM_ downloadLinks' $ decodeList x

downloadLinks' :: SermonsFile -> Html
downloadLinks' (SermonsFile _ "audio" path) = a ! href (textValue path) $ i ! class_ "fa fa-headphones" $ ""
downloadLinks' (SermonsFile _ "video" path) = a ! href (textValue path) $ i ! class_ "fa fs-film" $ ""
downloadLinks' (SermonsFile _ "text" path) = a ! href (textValue path)  $ i ! class_ "fa fa-download" $ ""
downloadLinks' (SermonsFile _ _ path) = a ! href (textValue path)  $ i ! class_  "fa fa-download" $ ""

updateGetParam :: [(Text,Text)] -> (Text,Text) -> Text
updateGetParam getParams (p, n) = (T.cons '?') . T.intercalate "&"
                                  . Import.map (\(k,v) -> k `T.append` "=" `T.append` v)
                                  . (++ [(p, n)]) . Import.filter ((/= p) . Import.fst) $ getParams