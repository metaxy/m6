module Handler.Article where

import Import
import Data.Maybe (listToMaybe)

textModule title_ text_ icon_  = do
    widgetToPageContent $ do 
        let text = text_ :: Text
            title = title_ :: Text
            icon = icon_ :: Text
        $(widgetFile "toolbar-text-module")
        
dispatchModules "hellersdorf" "start" = do
    module1 <- textModule "Anfahrt" "Hallo" "icon-calendar"
    module2 <- textModule "Anfahrt" "Hallo" "icon-calendar"
    module3 <- textModule "Anfahrt" "Hallo" "icon-calendar"
    module4 <- textModule "Anfahrt" "Hallo" "icon-calendar"
    widgetToPageContent $ do
        $(widgetFile "toolbar")

dispatchModules _ _ = do
    module1 <- textModule "Anfahrt" "Hallo" "icon-calendar"
    module2 <- textModule "Anfahrt" "Hallo" "icon-calendar"
    module3 <- textModule "Anfahrt" "Hallo" "icon-calendar"
    module4 <- textModule "Anfahrt" "Hallo" "icon-calendar"
    widgetToPageContent $ do 
        $(widgetFile "toolbar")


        
getArticleR :: Text -> Text -> Handler Html
getArticleR catAlias alias = do 
    catId <- runDB $ getBy404 $ UniqueAlias catAlias
    mayBeArticle <- runDB $ listToMaybe <$> selectList [ArticleAlias ==. alias, ArticleCatId ==. (entityKey catId)] []
    modules <- dispatchModules catAlias alias
    article <- case mayBeArticle of
        Just a -> do 
            return a 
        Nothing -> do 
            notFound -- show 404
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle $ entityVal article
        $(widgetFile "Article")
       
   
