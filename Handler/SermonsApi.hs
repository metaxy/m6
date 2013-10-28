module Handler.SermonsApi where

import Import
import Prelude
        
getSermonsApiR :: String -> Handler Html
getSermonsApiR file = do
    content <- liftIO $ readFile file
    defaultLayout $ do
        [whamlet|#{content}|]
    
    
    --read json file
    --insert json into db
