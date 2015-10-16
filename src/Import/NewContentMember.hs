-- | Helper form new content member forms.

module Import.NewContentMember where

import Import

data NewHelper value = NewHelper
  { newForm     :: (UserId -> Form value)
  , newConstr   :: (Key value -> Tcontent)
  , newGetTitle :: (value -> Title)
  , newWord     :: Html
  , newRedirect :: (UserHandle -> TutorialName -> TutorialNames -> Route App) -- ^ redirect dest
  , newTns      :: [TutorialName]
  }

-- | A helper for creating new content members and viewing the form for it.
getNewHelper :: (PersistEntity value, PersistEntityBackend value ~ SqlBackend)
             => Profile
             -> NewHelper value
             -> Handler Html
getNewHelper profile@Profile {..} NewHelper{..} = do
    ((res, widget), enctype) <- runFormPost $ newForm profileUser
    case res of
        FormSuccess value -> createNewHelper profile newConstr newGetTitle (Just newWord) newRedirect newTns (return ()) value
        _ -> defaultLayout $ do
            setTitle $ "Create new " ++ newWord
            let groupURL =
                    case newTns of
                        [] -> UserR profileHandle
                        x:xs -> UserTutorialR profileHandle x xs
            [whamlet|
                <h1>Create #{newWord}
                <form #create-form method=post enctype=#{enctype}>
                    <table>
                        ^{widget}
                        <tr>
                            <td colspan=2>
                                <button>Create
                                <a href=@{groupURL}>Cancel
            |]
            toWidget
                [lucius|
                    #create-form {
                        textarea { height: 200px }
                        input, textarea { width: 500px }
                    }
                |]

-- | A helper for only creating new content members.
createNewHelper :: (PersistEntity value, PersistEntityBackend value ~ SqlBackend) -- FIXME dedupe code with FP.IsolationHelper.CreateProject
                => Profile
                -> (Key value -> Tcontent)
                -> (value -> Title)
                -> Maybe Html -- ^ word to be used for setMessage, if any
                -> (UserHandle -> TutorialName -> TutorialNames -> Route App) -- ^ redirect dest
                -> [TutorialName]
                -> ReaderT (Entity value) Handler ()
                -> value
                -> Handler a
createNewHelper Profile {..} constr getTitle mword' redirect' tns cont value = do
   Entity holderid _ <- $runDB $ getNamesHolder True profileUser tns
                    >>= either (const $ getTopHolder profileUser) return
   (slug, ent) <- $runDB $ do
       vid <- insert value
       cid <- insert $ constr vid
       updateWhere [TmemberHolder ==. holderid] [TmemberPriority +=. 1]
       slug <- getUniqueSlug holderid $ getTitle value
       insert_ Tmember
           { tmemberContent = cid
           , tmemberHolder = holderid
           , tmemberSlug = slug
           , tmemberSlugUserGen = False
           , tmemberPriority = 0
           }
       return (slug, Entity vid value)
   runReaderT cont ent
   case mword' of
       Just word' -> setMessage $ "New " ++ word' ++ " created"
       Nothing -> return ()
   redirect $ case tns of
       [] -> redirect' profileHandle slug []
       x:xs -> redirect' profileHandle x $ xs ++ [slug]
