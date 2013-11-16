module Handler.BackendSermonSpeakerAdd where

import Import
import Yesod.Form.Nic (YesodNic,nicHtmlField)

entryForm :: Form SermonsSpeaker
entryForm = renderDivs $ SermonsSpeaker
    <$> areq textField "Name" Nothing
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
            setMessage $ toHtml $ (sermonsSpeakerName d) <> " created"
            redirect $ BackendSermonSpeakerListR
         _ -> backendDefaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "BackendAdd")
