module Handler.SermonsList where

import Import
import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html
import Text.Blaze.Internal
import Data.Time.Clock

formatScripture' :: SermonsScripture -> Html
formatScripture' (SermonsScripture book c1 v1 c2 v2 Nothing) = 
    a $ toHtml $ (show book) ++ " " ++(show c1) ++ ":" ++(show v1)
formatScripture' (SermonsScripture book c1 v1 c2 v2 (Just text))
    | T.null text = formatScripture' (SermonsScripture book c1 v1 c2 v2 Nothing)
    | otherwise = a $ toHtml text
    
formatScripture :: [SermonsScripture] -> Html
formatScripture x = mapM_ formatScripture' x

formatTime :: Maybe UTCTime -> Html
formatTime Nothing = toHtml (show "-")
formatTime (Just x) = toHtml $ show $ utctDay x

downloadLinks :: [SermonsFile] -> Html
downloadLinks = mapM_ downloadLinks'

downloadLinks' :: SermonsFile -> Html
downloadLinks' (SermonsFile _ "audio" path) = a ! href (textValue path) $ i ! class_ "fa-headphones" $ ""
downloadLinks' (SermonsFile _ "video" path) = a ! href (textValue path) $ i ! class_ "fs-film" $ ""
downloadLinks' (SermonsFile _ "text" path) = a ! href (textValue path)  $ i ! class_ "fa-pencil-square-o" $ ""
downloadLinks' (SermonsFile _ _ path) = a ! href (textValue path)  $ i ! class_  "fa-download" $ ""


getSermonsListR :: Text -> Text -> Handler Html
getSermonsListR cat groupAlias = do
    grp <- runDB $ getBy404 $ UniqueGroupAlias groupAlias
    sermons <- runDB $ selectList [SermonGroupId ==. entityKey grp] []
    defaultLayout $ do $(widgetFile "SermonList")


postSermonsListR :: Text -> Text -> Handler Html
postSermonsListR = error "Not yet implemented: postSermonsListR"
