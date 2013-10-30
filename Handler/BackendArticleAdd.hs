module Handler.BackendArticleAdd where

import Import
import Data.Time
import Yesod.Form.Nic (YesodNic,nicHtmlField)

getCategories :: Handler (OptionList (Entity Category)) 
getCategories = optionsPersist [] [Asc CategoryName] categoryName

entryForm :: Form Article
entryForm = renderBootstrap $ Article
    <$> areq textField "Title" Nothing
    <*> areq nicHtmlField "Content" Nothing
    <*> areq textField "Alias" Nothing
    <*> (entityKey <$> areq (selectField getCategories) "Category" Nothing)
    <*> lift (liftIO getCurrentTime)
    
    
getBackendArticleAddR :: Handler Html
getBackendArticleAddR = do
    (widget, enctype) <- generateFormPost entryForm
    defaultLayout $ do $(widgetFile "BackendAdd")


postBackendArticleAddR :: Handler Html
postBackendArticleAddR = do
    ((res,widget),enctype) <- runFormPost entryForm
    case res of
         FormSuccess article -> do
            articleId <- runDB $ insert article
            setMessage $ toHtml $ (articleTitle article) <> " created"
            redirect $ BackendArticleListR
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "BackendAdd")
