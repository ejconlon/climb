{-# LANGUAGE OverloadedStrings #-}

-- | A GHCI-like REPL with colon-commands.
module Climb
  ( Command
  , Completion
  , OptionCommands
  , ReplDef (..)
  , CommandExc (..)
  , bareCommand
  , runReplDef
  ) where

import Control.Exception (Exception)
import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Typeable (Typeable)
import qualified Data.ByteString.Char8 as BSC
import Linenoise.Repl (ReplDirective (..), replM)

type Command m = ByteString -> m ReplDirective

type OptionCommands m = [(ByteString, (ByteString, Command m))]

type Completion m = ByteString -> m [ByteString]

data CommandExc
  = ExpectedNoInputError
  | MissingCommandError !ByteString
  deriving (Eq, Show, Typeable)

instance Exception CommandExc

data ReplDef m =
  ReplDef
    { _rdGreeting :: !ByteString
    , _rdPrompt :: !ByteString
    , _rdOptionCommands :: !(OptionCommands m)
    , _rdExecCommand :: !(Command m)
    , _rdCompletion :: !(Completion m)
    }

assertEmpty :: MonadThrow m => ByteString -> m ()
assertEmpty input = unless (BSC.null input) (throwM ExpectedNoInputError)

bareCommand :: MonadThrow m => m ReplDirective -> Command m
bareCommand act input = assertEmpty input >> act

quitCommand :: MonadThrow m => Command m
quitCommand = bareCommand (pure ReplQuit)

helpCommand :: (MonadThrow m, MonadIO m) => OptionCommands m -> Command m
helpCommand opts = bareCommand $ do
  liftIO (BSC.putStrLn "Available commands:")
  for_ opts $ \(name, (desc, _)) -> liftIO (BSC.putStrLn (":" <> name <> "\t" <> desc))
  pure ReplContinue

defaultOptions :: (MonadThrow m, MonadIO m) => OptionCommands m -> OptionCommands m
defaultOptions opts =
    [ ("quit", ("quit", quitCommand))
    , ("help", ("describe all commands", helpCommand opts))
    ]

outerCommand :: MonadThrow m => OptionCommands m -> Command m -> Command m
outerCommand opts exec = \input ->
  case BSC.uncons input of
    Just (':', rest) -> do
      let (name, subInput) = BSC.break (==' ') rest
      case lookup name opts of
        Nothing -> throwM (MissingCommandError name)
        Just (_, command) -> command (BSC.drop 1 subInput)
    _ -> exec input

runReplDef :: (MonadThrow m, MonadUnliftIO m) => ReplDef m -> m ()
runReplDef (ReplDef greeting prompt opts exec comp) = do
  let allOpts = fix (\c -> defaultOptions c <> opts)
      action = outerCommand allOpts exec
  liftIO (BSC.putStrLn greeting)
  liftIO (BSC.putStrLn "Enter `:quit` to exit or `:help` to see all commands.")
  replM prompt action comp
