module Handler.DeleteAccount where

-- Commented out until SoH is the only user of the database

-- import Import
-- import Yesod.Auth (onLogout, clearCreds)

-- getDeleteAccountR :: Handler ()
-- getDeleteAccountR =
--     uid <- requireAuthId
--     -- Insert a deletion token into the database.  These deletion
--     -- tokens are garbage collected after one hour.  This token
--     -- protects against CSRF and enforces that the user
--     -- reauthenticates.
--     now <- liftIO getCurrentTime
--     token <- DeletionToken <$> randomString
--     _ <- $runDB $ upsert (UserDeletion uid now token) []
--     -- Logout the user session so that we can reauthenticate.
--     doLogout
--     -- Redirect to confirmation page after reauthenticating.
--     setUltDest (ConfirmDeleteAccountR token)
--     setMessage "Please re-authenticate in order to delete your account."
--     redirect (AuthR LoginR)

-- getConfirmDeleteAccountR :: DeletionToken -> Handler Html
-- getConfirmDeleteAccountR token = checkToken token $ defaultLayout $ do
--     setTitle "Confirm Account Deletion"
--     let deleteAccountWarning = $(widgetFile "delete-account-warning")
--     $(widgetFile "delete-account")

-- postConfirmDeleteAccountR :: DeletionToken -> Handler Html
-- postConfirmDeleteAccountR token = checkToken token $ do
--     uid <- requireAuthId
--     $runDB $ deleteUser uid
--     doLogout
--     defaultLayout $ do
--         setTitle "Account Deletion Confirmed"
--         [whamlet| <h1> Account successfully deleted. |]

-- doLogout :: Handler ()
-- doLogout = do
--     onLogout
--     clearCreds False

-- checkToken :: DeletionToken -> Handler Html -> Handler Html
-- checkToken token f = do
--     uid <- requireAuthId
--     mres <- $runDB $ getBy $ UniqueUserDeletion uid
--     case mres of
--         Just (Entity _ ud) | userDeletionToken ud == token -> f
--         _ -> defaultLayout $ do
--             setTitle "Error: Account Not Deleted"
--             [whamlet|
--                 <h1>Error: Account Not Deleted
--                 <p>Your account has not been deleted, since #
--                  the confirmation token is invalid.  Please #
--                  <a href=@{DeleteAccountR}>try again</a>.
--               |]
