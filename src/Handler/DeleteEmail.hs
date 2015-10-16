module Handler.DeleteEmail where

import Import

deleteDeleteEmailR :: IdentId -> Handler ()
deleteDeleteEmailR iid = do
    uid <- requireAuthId
    $runDB $ do
        Ident {..} <- get404 iid
        unless identEmail $ lift notFound
        unless (identUser == uid) $ lift notFound
        delete iid
        lift $ setMessage $ toHtml $ "Email address deleted: " ++ identIdent
    redirect ProfileR
