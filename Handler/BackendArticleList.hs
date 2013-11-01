module Handler.BackendArticleList where

import Import

getBackendArticleListR :: Handler Html
getBackendArticleListR = do  
    articles <- runDB $ selectList [] [Desc ArticleTitle]
    backendDefaultLayout $ do $(widgetFile "BackendArticleList")

