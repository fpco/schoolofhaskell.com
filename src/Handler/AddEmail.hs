module Handler.AddEmail where

import Import
import Text.Email.Validate (canonicalizeEmail)
import Yesod.Auth.Email (randomKey)
import Network.Mail.Mime
import Text.Shakespeare.Text (textFile)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Time (addUTCTime)

postAddEmailR :: Handler ()
postAddEmailR = do
    uid <- requireAuthId
    memail <- lookupPostParam "email"
    case memail >>= canonicalizeEmail . encodeUtf8 . toLower of
        Nothing -> setMessage "Invalid email address"
        Just (decodeUtf8 -> email) -> do
            mident <- $runDB $ getUserByEmail False email
            case mident of
                Just _ -> setMessage "That email address is already in use"
                Nothing -> do
                    key <- getYesod >>= liftIO . randomKey
                    now <- liftIO getCurrentTime
                    let expires = (60 * 60 * 2) `addUTCTime` now
                    ceid <- $runDB $ insert $ ConfirmEmail uid (mkLowerCaseText email) expires key
                    let verurl = ConfirmEmailR ceid key

                    y <- getYesod
                    render <- getUrlRenderParams
                    eres <- liftResourceT $ try $ appRenderSendMail y (httpManager y) email $ \from -> (emptyMail from)
                        { mailTo = [Address Nothing email]
                        , mailHeaders =
                            [ ("Subject", "School of Haskell: Confirm email address")
                            ]
                        , mailParts = return $ return Part
                            { partType = "text/plain; charset=utf-8"
                            , partEncoding = None
                            , partFilename = Nothing
                            , partContent = encodeUtf8 $ toLazyText $ $(textFile "templates/confirm-email.txt") render
                            , partHeaders = []
                            }
                        }
                    case eres of
                        Left e@StatusCodeException{} ->
                            error $ "Seemingly invalid email address: " ++ show email ++ "\n\n" ++ show e
                        Left e -> throwIO e
                        Right () -> setMessage "A confirmation email has been sent"

    redirect ProfileR
