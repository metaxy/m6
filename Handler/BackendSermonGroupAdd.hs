module Handler.BackendSermonGroupAdd where

import Import
getCategories :: Handler (OptionList (Entity Category)) 
getCategories = optionsPersist [] [Asc CategoryTitle] categoryTitle 

entryForm :: Form SermonGroup
entryForm = renderDivs $ SermonGroup
    <$> areq textField "Name" Nothing
    <*> areq textField "Alias" Nothing
    <*> (entityKey <$> areq (selectField getCategories) "Category" Nothing)
    
getBackendSermonGroupAddR :: Handler Html
getBackendSermonGroupAddR = do
    (widget, enctype) <- generateFormPost entryForm
    defaultLayout $ do $(widgetFile "BackendAdd")

postBackendSermonGroupAddR :: Handler Html
postBackendSermonGroupAddR = do
    ((res,widget),enctype) <- runFormPost entryForm
    case res of
         FormSuccess d -> do
            itemId <- runDB $ insert d
            setMessage $ toHtml $ (sermonGroupName d) <> " created"
            redirect $ BackendSermonGroupListR
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "BackendAdd")
