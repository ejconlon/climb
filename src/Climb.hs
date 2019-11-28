{-# LANGUAGE OverloadedStrings #-}

-- | Building blocks for a GHCI-like REPL with colon-commands.
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

-- | A 'Command' takes some input, performs some effect, and returns a directive (continue or quit).
type Command m = ByteString -> m ReplDirective

-- | List of 'Command's by name with help text.
type OptionCommands m = [(ByteString, (ByteString, Command m))]

-- | A 'Completion' takes some input and returns potential matches.
type Completion m = ByteString -> m [ByteString]

-- | Sometimes things go wrong...
data CommandExc
  = ExpectedNoInputError
  -- ^ An option 'Command' got input when it expected None
  | MissingCommandError !ByteString
  -- ^ An option 'Command' was not found by name.
  deriving (Eq, Show, Typeable)

instance Exception CommandExc

-- | Defines a REPL with commands, options, and completion.
data ReplDef m =
  ReplDef
    { _rdOnInterrupt :: !ReplDirective
    , _rdGreeting :: !ByteString
    , _rdPrompt :: !ByteString
    , _rdOptionCommands :: !(OptionCommands m)
    , _rdExecCommand :: !(Command m)
    , _rdCompletion :: !(Completion m)
    }

assertEmpty :: MonadThrow m => ByteString -> m ()
assertEmpty input = unless (BSC.null input) (throwM ExpectedNoInputError)

-- | Helps you define commands that expect no input.
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

-- | Runs a REPL as defined.
runReplDef :: (MonadThrow m, MonadUnliftIO m) => ReplDef m -> m ()
runReplDef (ReplDef onInterrupt greeting prompt opts exec comp) = do
  let allOpts = fix (\c -> defaultOptions c <> opts)
      action = outerCommand allOpts exec
  liftIO (BSC.putStrLn greeting)
  liftIO (BSC.putStrLn "Enter `:quit` to exit or `:help` to see all commands.")
  replM onInterrupt prompt action comp
