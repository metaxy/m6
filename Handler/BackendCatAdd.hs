module Handler.BackendCatAdd where

import Import
entryForm :: Form Category
entryForm = renderDivs $ Category
    <$> areq textField "Name" Nothing
    <*> areq textField "Alias" Nothing
    
getBackendCatAddR :: Handler Html
getBackendCatAddR = do
    (widget, enctype) <- generateFormPost entryForm
    backendDefaultLayout $ do $(widgetFile "BackendAdd")

postBackendCatAddR :: Handler Html
postBackendCatAddR = do
    ((res,widget),enctype) <- runFormPost entryForm
    case res of
         FormSuccess category -> do
            articleId <- runDB $ insert category
            setMessage $ toHtml $ (categoryName category) <> " created"
            redirect $ BackendCatListR
         _ -> backendDefaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "BackendAdd")
