module Handler.Article where

import Import
import Data.Maybe (listToMaybe)
getArticleR :: Text -> Text -> Handler Html
getArticleR catAlias alias = do 
    catId <- runDB $ getBy404 $ UniqueAlias catAlias
    mayBeArticle <- runDB $ listToMaybe <$> selectList [ArticleAlias ==. alias, ArticleCatId ==. (entityKey catId)] []
    article <- case mayBeArticle of
        Just a -> do 
            return a 
        Nothing -> do 
            notFound -- show 404
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle $ entityVal article
        $(widgetFile "Article")
       
   
