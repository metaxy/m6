module Handler.SermonsList where
import Model.SermonsTable

import Import
import qualified Data.Text as T

getSermonsListR :: Text -> Text -> Handler Html
getSermonsListR cat groupAlias = do
    grp <- runDB $ getBy404 $ UniqueGroupAlias groupAlias
    sermons' <- runDB $ selectList [SermonGroupId ==. entityKey grp] []
    table <- widgetToPageContent $ sermonsTable sermons'
    defaultLayout $ 
        do $(widgetFile "SermonList")


postSermonsListR :: Text -> Text -> Handler Html
postSermonsListR = error "Not yet implemented: postSermonsListR"
