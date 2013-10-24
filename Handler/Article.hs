module Handler.Article where

import Import
import Data.Maybe (listToMaybe)

findArticle a c = listToMaybe <$> selectList [ArticleAlias ==. a, ArticleCatId ==. c] []
getArticleR :: Text -> Text -> Handler Html
getArticleR catAlias alias = do 
    catId <- runDB $ getBy404 $ UniqueAlias catAlias
    --articles <- runDB $ selectList [ArticleAlias ==. alias, ArticleCatId ==. (entityKey catId)] []
    article2 <- runDB $ findArticle alias (entityKey catId)
    case article2 of
       Nothing -> do 
           notFound 
       Just article -> do
           defaultLayout $ do $(widgetFile "Article")
       
   
