module Handler.BackendSermonGroupAdd where

import Import

entryForm :: Form SermonsGroup
entryForm = renderDivs $ SermonsGroup
    <$> areq textField "Name" Nothing
    
getBackendSermonGroupAddR :: Handler Html
getBackendSermonGroupAddR = do
    (widget, enctype) <- generateFormPost entryForm
    backendDefaultLayout $ do $(widgetFile "BackendAdd")

postBackendSermonGroupAddR :: Handler Html
postBackendSermonGroupAddR = do
    ((res,widget),enctype) <- runFormPost entryForm
    case res of
         FormSuccess d -> do
            itemId <- runDB $ insert d
            setMessage $ toHtml $ (sermonsGroupName d) <> " created"
            redirect $ BackendSermonGroupListR
         _ -> backendDefaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "BackendAdd")
