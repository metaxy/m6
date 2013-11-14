module Handler.SermonsList where

import Import
{--formatSpeaker :: Maybe SermonSpeakerId -> Widget
formatSpeaker (Just x) = do
    a <- getBy $ UniqueGroupAlias "test"
    toWidget $ [hamlet| <a> |]

formatSpeaker _ = do
    toWidget $ [hamlet| <a> |]
    --}    
getSermonsListR :: Text -> Text -> Handler Html
getSermonsListR cat grp = do
    groupId <- fmap entityKey $ runDB $ getBy404 $ UniqueGroupAlias grp
    sermons <- runDB $ selectList [SermonSermonGroupId ==. groupId] []
    defaultLayout $ do $(widgetFile "SermonList")


postSermonsListR :: Text -> Text -> Handler Html
postSermonsListR = error "Not yet implemented: postSermonsListR"
