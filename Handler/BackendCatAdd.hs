module Handler.BackendCatAdd where

import Import

entryForm :: Form Category
entryForm = renderDivs $ Category
    <$> areq textField "Title" Nothing
    <*> areq textField "Alias" Nothing
    
getBackendCatAddR :: Handler Html
getBackendCatAddR = do
    (categoryWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do $(widgetFile "BackendCatAdd")

postBackendCatAddR :: Handler Html
postBackendCatAddR = do
    ((res,categoryWidget),enctype) <- runFormPost entryForm
    case res of
         FormSuccess category -> do
            articleId <- runDB $ insert category
            setMessage $ toHtml $ (categoryTitle category) <> " created"
            redirect $ BackendCatListR
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "BackendCatAddError")
