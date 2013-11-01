module Handler.BackendSermonSpeakerAdd where

import Import
import Yesod.Form.Nic (YesodNic,nicHtmlField)

entryForm :: Form SermonSpeaker
entryForm = renderDivs $ SermonSpeaker
    <$> areq textField "Name" Nothing
    <*> areq textField "Alias" Nothing
    <*> aopt textField "Picture" Nothing
    <*> aopt nicHtmlField "Biographie" Nothing
    
getBackendSermonSpeakerAddR :: Handler Html
getBackendSermonSpeakerAddR = do
    (widget, enctype) <- generateFormPost entryForm
    backendDefaultLayout $ do $(widgetFile "BackendAdd")

postBackendSermonSpeakerAddR :: Handler Html
postBackendSermonSpeakerAddR = do
    ((res,widget),enctype) <- runFormPost entryForm
    case res of
         FormSuccess d -> do
            itemId <- runDB $ insert d
            setMessage $ toHtml $ (sermonSpeakerName d) <> " created"
            redirect $ BackendSermonSpeakerListR
         _ -> backendDefaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "BackendAdd")
