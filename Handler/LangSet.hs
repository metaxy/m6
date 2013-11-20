module Handler.LangSet where

import Import

getLangSetR :: Text -> Handler ()
getLangSetR lang = do
    setUltDestReferer
    setLanguage lang
    redirectUltDest HomeR