{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module FP.Logger
    ( useSyslog
    , withSyslog
    , runSyslogLoggingT
    , runSyslogShouldLoggingT
    , syslogMessageLogger
    ) where

import           BasicPrelude
import           Control.Monad.Logger       (LogLevel (..), LogSource, runLoggingT, LoggingT)
import qualified Data.Text                  as Text
import           FileLocation               (locationToString)
import           FP.EnvSettings             (fpLogStderr)
import           Language.Haskell.TH.Syntax (Loc)
import qualified Prelude
import           System.Log.FastLogger      (LogStr, fromLogStr)
import           System.Posix.Syslog        (Facility (..), Option (..), syslog, withSyslog)
import qualified System.Posix.Syslog        as Syslog

-- | 'System.Posix.Syslog.useSyslog' with different defaults.  Uses LOCAL1 facility
-- and the FP_LOG_NOSTDERR environment variable to determine whether to use the PERROR option.
-- You must wrap your @main@ with this.
useSyslog :: String -> IO a -> IO a
useSyslog ident action = withSyslog ident (PID : [PERROR | fpLogStderr]) LOCAL1 [minBound..maxBound] action

-- | Run a block using a MonadLogger instance which sends messages to the syslog service
runSyslogLoggingT :: MonadIO m => LoggingT m a -> m a
runSyslogLoggingT action =
    runLoggingT action $ syslogMessageLogger (\_ _ -> return True)

-- | Run a block using a MonadLogger instance which sends messages to the syslog service, with
-- filtering of the messages sent
runSyslogShouldLoggingT :: MonadIO m => (LogSource -> LogLevel -> IO Bool) -> LoggingT m a -> m a
runSyslogShouldLoggingT shouldLogIO action =
    runLoggingT action $ syslogMessageLogger shouldLogIO

-- | monad-logger message formatter that logs to syslog.  See 'Control.Monad.Logger.runLoggingT'.
syslogMessageLogger :: (LogSource -> LogLevel -> IO Bool)
                    -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
syslogMessageLogger shouldLogIO loc source level msg = do
    sl <- shouldLogIO source level
    when sl $ do
        let s = concat
                [ drop 5 $ Prelude.show level
                , if Text.null source
                    then ""
                    else '#' : Text.unpack source
                , ": "
                , Text.unpack $ decodeUtf8 $ fromLogStr msg
                , " @"
                , locationToString loc ]
            p =
                case level of
                    LevelError   -> Syslog.Error
                    LevelDebug   -> Syslog.Debug
                    LevelInfo    -> Syslog.Info
                    LevelWarn    -> Syslog.Warning
                    LevelOther _ -> Syslog.Notice
        syslog p s
