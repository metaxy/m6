module Handler.SermonsList where

import Import

getSermonsListR :: Text -> Text -> Handler Html
getSermonsListR cat alias = do
    sermons <- runDB $ selectList [SermonSermonAlias ==. alias] []
    defaultLayout $ do $(widgetFile "SermonList")
    
postSermonsListR :: Text -> Text -> Handler Html
postSermonsListR = error "Not yet implemented: postSermonsListR"
