module Handler.Lang where

import Import
import Yesod.Core

postLangR :: Handler ()
postLangR = do
    setUltDestReferer
    lang <- runInputPost $ ireq textField "lang"
    setLanguage lang
    redirectUltDest HomeR
