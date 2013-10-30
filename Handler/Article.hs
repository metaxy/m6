module Handler.Article where

import Import
import Data.Maybe (listToMaybe)
textModule :: Text -> Text -> Text -> Widget
textModule title text icon  = do
    toWidget $(widgetFile "toolbar-text-module")

dispatchModules :: Text -> Text -> Maybe Widget
dispatchModules "hellersdorf" "start" = do
    let module1 = textModule "Anfahrt" "Hallo" "icon-calendar"
    let module2 = textModule "Anfahrt" "Hallo" "icon-calendar"
    let module3 = textModule "Anfahrt" "Hallo" "icon-calendar"
    let module4 = textModule "Anfahrt" "Hallo" "icon-calendar"
    Just (toWidget $(widgetFile "toolbar"))

dispatchModules _ _ = Nothing

dispatchSecondMenu :: Text -> Text -> Widget
--secondMenu "hellersdorf" _ = toWidget $(widgetFile "second-menu-hellersdorf")
dispatchSecondMenu _ _ = toWidget $(widgetFile "second-menu-hellersdorf")

secondMenu :: Text -> Text -> Widget
secondMenu c a = do
    let menu = dispatchSecondMenu c a
    toWidget $(widgetFile "second-menu")
 
getArticleR :: Text -> Text -> Handler Html
getArticleR catAlias alias = do 
    catId <- runDB $ getBy404 $ UniqueAlias catAlias
    mayBeArticle <- runDB $ listToMaybe <$> selectList [ArticleAlias ==. alias, ArticleCatId ==. (entityKey catId)] []
    sMenu <- widgetToPageContent $ secondMenu catAlias alias
    let a = dispatchModules catAlias alias
    mTop <- case a of
        Just b -> do
            c <-  widgetToPageContent $ toWidget b
            widgetToPageContent $ toWidget [hamlet|
            <div .dark-bg>
                ^{pageBody sMenu} 
                ^{pageBody c}|]
        Nothing -> do
            widgetToPageContent $ toWidget [hamlet|^{pageBody sMenu}|]
    
    article <- case mayBeArticle of
        Just article -> do 
            return article
        Nothing -> do 
            notFound -- show 404
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle $ entityVal article
        $(widgetFile "Article")
       
   
