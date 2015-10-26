{-# LANGUAGE CPP #-}

module Settings.StaticFiles where

import ClassyPrelude.Yesod
import qualified Yesod.Static as Static
import Settings.Development
import Language.Haskell.TH.Syntax
import Settings (staticDir)

-- | use this to create your static file serving site
staticSite :: IO Static.Static
staticSite = if development then Static.staticDevel staticDir
                            else Static.static      staticDir

-- | This generates easy references to files in the static directory at compile time,
--   giving you compile-time verification that referenced files exist.
--   Warning: any files added to your static directory during run-time can't be
--   accessed this way. You'll have to use their FilePath or URL to access them.
staticFilesList Settings.staticDir
    [ "codemirror/LICENSE.txt"
    , "codemirror/mode/markdown/markdown.js"
    , "codemirror/mode/javascript/javascript.js"
    , "codemirror/mode/haskell/haskell.js"
    , "codemirror/mode/haskell/shakespeare/shakespeare.js"
    , "codemirror/mode/haskell/routes.js"
    , "codemirror/mode/haskell/shakespeare/css.js"
    , "codemirror/mode/haskell/shakespeare/lucius.js"
    , "codemirror/mode/haskell/shakespeare/cassius.js"
    , "codemirror/mode/haskell/shakespeare/julius.js"
    , "codemirror/mode/haskell/shakespeare/hamlet.js"
    , "codemirror/mode/haskell/shakespeare/yesod.js"
    , "codemirror/mode/clike/clike.js"
    , "codemirror/addon/mode/multiplex.js"
    , "codemirror/addon/runmode/runmode.js"
    , "codemirror/mode/css/css.js"
    , "codemirror/mode/xml/xml.js"
    , "codemirror/mode/htmlmixed/htmlmixed.js"
    , "codemirror/mode/yaml/yaml.js"
    , "codemirror/keymap/emacs.js"
    , "codemirror/keymap/vim.js"
    , "codemirror/lib/codemirror.js"
    , "codemirror/lib/codemirror.css"
    , "codemirror/addon/dialog/dialog.css"
    , "codemirror/addon/dialog/dialog.js"
    , "codemirror/addon/edit/closebrackets.js"
    , "codemirror/addon/edit/matchbrackets.js"
    , "codemirror/addon/edit/trailingspace.js"
    , "codemirror/addon/hint/show-hint.css"
    , "codemirror/addon/hint/show-hint.js"
    , "codemirror/addon/search/search.js"
    , "codemirror/addon/search/searchcursor.js"
    , "codemirror/addon/search/match-highlighter.js"
    , "codemirror/addon/selection/active-line.js"
    , "img/close.png"
    , "img/showSolution.png"
    , "img/hideSolution.png"
    , "img/loadingSmall.gif"
    , "img/loading.gif"
    , "design/img/tmp/big.png"

    , "js/fpco-codemirror.js"

    -- BlockUI lightbox
    , "js/blockUI.js"

    -- JQuery UI 1.10 with draggable / droppable / resizable and their dependencies ONLY
    , "js/jquery-ui.custom.js"
    , "js/jquery.ui.touch-punch.min.js"
    -- JQuery sortable
    , "js/jquery-sortable.js"

    -- JQuery backup file
    , "js/jquery.js"

    -- JQuery Validation
    , "js/jquery.validationEngine.js"
    , "js/jquery.validationEngine-en.js"
    , "css/validationEngine.jquery.css"

    -- IcoMoon Icons
    , "css/icomoon.css"
    , "fonts/IcoMoon.eot"
    , "fonts/IcoMoon.svg"
    , "fonts/IcoMoon.ttf"
    , "fonts/IcoMoon.woff"

    -- Design
    -- TODO: Strip this redundant stuff out and remove the files.
    , "design/css/style.css"
    , "design/css/ie7.css"
    , "design/css/normalize.css"
    , "design/fonts/fonts.css"
    , "design/js/init.js"
    , "design/js/ie.js"
    , "design/img/ico/tw.png"
    , "design/img/ico/go.png"
    , "design/img/ico/fb.png"
    , "design/img/bg/menu.png"

    -- Bootstrap
    , "bootstrap/css/bootstrap.css"
    , "bootstrap/js/bootstrap.js"
    , "font-awesome/font/fontawesome-webfont.svg"
    , "font-awesome/font/fontawesome-webfont.eot"
    , "font-awesome/font/fontawesome-webfont.ttf"
    , "font-awesome/font/fontawesome-webfont.woff"
    , "font-awesome/font/FontAwesome.otf"
    , "font-awesome/css/font-awesome.css"
    ]

combineSettings :: CombineSettings
combineSettings = def
    { csStaticDir = fpFromString staticDir
    }

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets' development combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts' development combineSettings
