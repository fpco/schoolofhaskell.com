module Handler.Dashboard where

import Import

getDashboardR :: Handler Html
getDashboardR = do
    Entity _ profile <- requireProfile
    redirect $ UserR $ profileHandle profile
