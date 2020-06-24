{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Climb
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO (..), wrappedWithRunInIO)
import Linenoise

newtype Repl a = Repl { unRepl :: ReplT () () IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

instance MonadUnliftIO Repl where
  withRunInIO = wrappedWithRunInIO Repl unRepl

runRepl :: Repl a -> IO a
runRepl r = fmap fst (runReplT (unRepl r) () ())

options :: OptionCommands Repl
options = mempty

exec :: Command Repl
exec = const (pure ReplContinue)

completion :: Completion Repl
completion = const (pure [])

replDef :: ReplDef Repl
replDef = ReplDef
  { _rdOnInterrupt = ReplContinue
  , _rdGreeting = "Hello, REPL!"
  , _rdPrompt = "> "
  , _rdOptionCommands = options
  , _rdExecCommand = exec
  , _rdCompletion = completion
  }

main :: IO ()
main = runRepl (runReplDef replDef)
