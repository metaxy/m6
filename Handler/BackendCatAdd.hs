module Handler.BackendCatAdd where

import Import
entryForm :: Form Category
entryForm = renderDivs $ Category
    <$> areq textField "Title" Nothing
    <*> areq textField "Alias" Nothing
    
getBackendCatAddR :: Handler Html
getBackendCatAddR = do
    (widget, enctype) <- generateFormPost entryForm
    defaultLayout $ do $(widgetFile "BackendAdd")

postBackendCatAddR :: Handler Html
postBackendCatAddR = do
    ((res,widget),enctype) <- runFormPost entryForm
    case res of
         FormSuccess category -> do
            articleId <- runDB $ insert category
            setMessage $ toHtml $ (categoryTitle category) <> " created"
            redirect $ BackendCatListR
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "BackendAdd")
