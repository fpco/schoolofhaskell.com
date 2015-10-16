{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Handler.Profile where

import Import hiding (handle)
import Yesod.Auth.Email (setpassR, needOldPassword, getPassword, forgotPasswordR)

handleField :: Field Handler (UserHandle, NormalizedHandle)
handleField =
    checkMMap checkHandle (unUserHandle . fst) textField
  where
    checkHandle :: Text -> Handler (Either Text (UserHandle, NormalizedHandle))
    checkHandle (UserHandle -> uh) =
        case normalizeHandle uh of
            Left e -> return $ Left $ tshow e
            Right nh -> do
                mx <- $runDB $ getBy $ UniqueNormalizedHandle nh
                return $ case mx of
                    Nothing -> Right (uh, nh)
                    Just _ -> Left "Username in use"

profileForm :: Profile -> Form Profile
profileForm profile = renderTable $ mkProfile
    <$> (if profileAutomatic profile
            then areq handleField "Username" { fsId = Just "handle", fsAttrs = [("autofocus", "")] } Nothing
            else pure (profileHandle profile, profileNormalizedHandle profile))
    <*> (aopt textField "Real Name" (Just $ Just $ profileDisplay profile))
    <*> aopt (selectField $ optionsPairs keymaps) "Editor Keymappings" (Just $ profileKeymap profile)
    <*> aopt textareaField "Personal Bio" (Just $ profileBio profile)
    <*> (fromMaybe "" <$> aopt textField "Company" (Just $ Just $ profileCompany profile))
    <*> aopt urlField "Homepage" (Just $ profileHomepage profile)
    <*> aopt textField "Telephone" (Just $ profileTelephone profile)
    <*> disqusField (profileDisqus profile)
  where
    mkProfile (handle', normalizedHandle) mdisplay keymap bio company homepage telephone disqus = profile
        { profileHandle = handle'
        , profileNormalizedHandle = normalizedHandle
        , profileDisplay = fromMaybe (unUserHandle handle') mdisplay
        , profileKeymap = keymap
        , profileBio = bio
        , profileCompany = company
        , profileHomepage = homepage
        , profileTelephone = telephone
        , profileAutomatic = False
        , profileDisqus = disqus
        }
    keymaps = asList $ map goKeymap [minBound..maxBound]
    goKeymap x = (showKeymap x, x)

    showKeymap :: Keymap -> Text
    showKeymap KeymapVim = "Vim"
    showKeymap KeymapEmacs = "Emacs"

disqusField :: Maybe Disqus -> AForm Handler (Maybe Disqus)
disqusField orig = formToAForm $ do
    disqusSettings <- newIdent
    radioId <- newIdent
    radioName <- newIdent
    textName <- newIdent

    radio1 <- newIdent
    radio2 <- newIdent
    radio3 <- newIdent
    radio4 <- newIdent

    menv <- askParams
    let checkUserAccount (Just (UserAccount _)) = True
        checkUserAccount _ = False
        success x = (checkUserAccount x, x, FormSuccess x)
        lookup1 x = do
            env <- menv
            [y] <- lookup x env
            return y
        (isUserAccount, curr, res) =
            case lookup1 radioName of
                Nothing -> (checkUserAccount orig, orig, FormMissing)
                Just "default" -> success Nothing
                Just "nocomments" -> success $ Just NoComments
                Just "fpaccount" -> success $ Just FPAccount
                Just "useraccount" ->
                    case lookup1 textName of
                        Nothing -> (True, Nothing, FormFailure ["No account ID provided"])
                        Just di' ->
                            case mkDisqusIdent di' of
                                Left e -> (True, Nothing, FormFailure ["Invalid Disqus ID: " ++ tshow e])
                                Right di -> success $ Just $ UserAccount di
                Just x -> (False, orig, FormFailure ["Invalid radio value: " ++ tshow x])

        userAccount :: Text
        userAccount =
            case curr of
                Just (UserAccount x) -> unDisqusIdent x
                _ -> fromMaybe "" $ lookup1 textName

        view = FieldView
            { fvLabel = "Disqus settings"
            , fvTooltip = Nothing
            , fvId = radioId
            , fvInput = $(widgetFile "disqus-field")
            , fvErrors =
                case res of
                    FormMissing -> Nothing
                    FormSuccess _ -> Nothing
                    FormFailure e -> Just $ toHtml $ intercalate "\n" e
            , fvRequired = False
            }
    return (res, [view])

getProfileR :: Handler Html
getProfileR = do
    user <- requireAuth
    Entity pid profile0 <- requireProfile
    emails <- $runDB $ selectList
        [ IdentUser ==. entityKey user
        , IdentIdent !=. userEmail (entityVal user)
        , IdentEmail ==. True
        ]
        [ Asc IdentIdent
        ]
    keypair <- $runDB $ getSSHKeyPair $ Entity pid profile0
    ((mres, widget), enctype) <- runFormPost $ profileForm profile0
    case mres of
        FormSuccess profile -> do
            $runDB $ replace pid profile
            let fullname = profileDisplay profile
                mfullname = if null fullname then Nothing else Just fullname
            setMessage "Profile updated."
            redirect ProfileR
        _ -> return ()
    needOldPassword' <- needOldPassword $ entityKey user
    hasNoPassword <- isNothing <$> getPassword (entityKey user)

    let deleteAccountWarning = $(widgetFile "delete-account-warning")
    defaultLayout $ do
        setTitle "My Account"
        $(widgetFile "profile")

putProfileR :: Handler Html
putProfileR = getProfileR
