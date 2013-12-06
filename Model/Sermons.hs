module Model.Sermons where

import Import
import qualified Data.Text as T
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html()
import Text.Blaze.Internal
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Data.Maybe

--import Data.Time.Clock
--import Data.Time.Calendar

lazyToStrictBS :: LBS.ByteString -> BS.ByteString
lazyToStrictBS x = BS.concat $ LBS.toChunks x

formatScripture' :: SermonsScripture -> Html
formatScripture' (SermonsScripture book c1 v1 c2 v2 Nothing) = 
    toHtml $ (show book) ++ " " ++(show c1) ++ ":" ++(show v1)
formatScripture' (SermonsScripture book c1 v1 c2 v2 (Just t))
    | T.null t = formatScripture' (SermonsScripture book c1 v1 c2 v2 Nothing)
    | otherwise = toHtml t
 
formatScripture :: [BS.ByteString] -> Html
formatScripture x = mapM_ formatScripture' $ catMaybes $ Import.map(decode . LBS.fromStrict) x 

formatTime :: Maybe Text -> Html
formatTime Nothing = toHtml $ show ""
formatTime (Just t) = toHtml $ t
--formatTime (Just x) = toHtml $ f $ toGregorian $ utctDay x
  --  where
--        f (a,b,c) = (show c) ++ "." ++ show(b) ++ "." ++ (show a)
       
downloadLinks :: [SermonsFile] -> Html
downloadLinks = mapM_ downloadLinks'

downloadLinks' :: SermonsFile -> Html
downloadLinks' (SermonsFile _ "audio" path) = a ! href (textValue path) $ i ! class_ "fa fa-headphones" $ ""
downloadLinks' (SermonsFile _ "video" path) = a ! href (textValue path) $ i ! class_ "fa fs-film" $ ""
downloadLinks' (SermonsFile _ "text" path) = a ! href (textValue path)  $ i ! class_ "fa fa-download" $ ""
downloadLinks' (SermonsFile _ _ path) = a ! href (textValue path)  $ i ! class_  "fa fa-download" $ ""

updateGetParam :: [(Text,Text)] -> (Text,Text) -> Text
updateGetParam getParams (p, n) = (T.cons '?') . T.intercalate "&"
                                  . Import.map (\(k,v) -> k `T.append` "=" `T.append` v)
                                  . (++ [(p, n)]) . Import.filter ((/= p) . Import.fst) $ getParams