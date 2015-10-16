module Handler.EditGroup where

import Import

form :: Entity Tmember -> Tgroup -> Form (Tgroup, TutorialName)
form memEnt@(Entity _ Tmember {..}) Tgroup {..} = renderTable $ (,) <$> (Tgroup
    <$> pure tgroupAuthor
    <*> areq titleField "Title" (Just tgroupTitle)
    <*> (fromMaybe (Textarea "") <$> aopt textareaField "Description" (Just $ Just tgroupDesc))
    <*> lift (liftIO getCurrentTime))
        <*> areq (slugField memEnt) "Slug" { fsTooltip = Just "Used in URLs" } (Just tmemberSlug)

slugField :: Entity Tmember
          -> Field Handler TutorialName
slugField (Entity mid Tmember {..}) =
    checkMMap checkUnique' unTutorialName textField
  where
    checkUnique' :: Text -> Handler (Either Text TutorialName)
    checkUnique' raw = $runDB $ do
        res <- getBy $ UniqueMember tmemberHolder slug
        return $ case res of
            Just (Entity mid2 _) | mid /= mid2 -> Left "The specified slug is already taken"
            _ -> Right slug
      where
        slug = TutorialName raw

getEditGroupR :: TutorialName -> TutorialNames -> Handler Html
getEditGroupR tn tns = do
    Entity _ Profile {..} <- requireProfile
    (Entity gid tgroup0, entMid@(Entity mid Tmember {..})) <- $runDB $ do
        eres <- followSlugPath profileUser tn tns
        (mid, Entity _ content, _) <- either (const notFound) return eres
        case content of
            TcontentGroupSum gid -> do
                g <- get404 gid
                m <- get404 mid
                return (Entity gid g, Entity mid m)
            _ -> notFound
    unless (tgroupAuthor tgroup0 == profileUser) notFound
    ((res, widget), enctype) <- runFormPost $ form entMid tgroup0
    case res of
        FormSuccess (tgroup, slug) -> do
            mslug <- $runDB $ do
                replace gid tgroup
                if slug == tmemberSlug
                    then return Nothing
                    else do
                        update mid
                            [ TmemberSlug =. slug
                            , TmemberSlugUserGen =. True
                            ]
                        return $ Just slug
            case mslug of
                Nothing -> return ()
                Just slug' ->
                    let (tn', tns') =
                            case reverse tns of
                                [] -> (slug', [])
                                _:rest -> (tn, reverse $ slug' : rest)
                     in redirect $ EditGroupR tn' tns'
        _ -> return ()
    defaultLayout $ do
        setTitle "Edit Group"
        [whamlet|
            <h1>Edit Group
            <form #edit-group method=post enctype=#{enctype}>
                <table>
                    ^{widget}
                    <tr>
                        <td colspan=2>
                            <button>Update
                            <a href=@{UserTutorialR profileHandle tn tns}>Return to group
            <form method=post action=@{DeleteMemberR tn tns} style=display:inline-block>
                <button>Delete
        |]
        toWidget
            [lucius|
                #edit-group {
                    textarea { height: 200px }
                    input, textarea { width: 500px }
                }
            |]

postEditGroupR :: TutorialName -> TutorialNames -> Handler Html
postEditGroupR = getEditGroupR
