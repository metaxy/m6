module Handler.SermonsList where

import Import

getSermonsListR :: Text -> Text -> Handler Html
getSermonsListR cat grp = do
    groupId <- runDB $ getBy404 $ UnqiueGroupAlias grp
    sermons <- runDB $ selectList [SermonSermonGroupId ==. groupId] []
    defaultLayout $ do $(widgetFile "SermonList")
    
postSermonsListR :: Text -> Text -> Handler Html
postSermonsListR = error "Not yet implemented: postSermonsListR"
