{-# LANGUAGE OverloadedStrings #-}

-- | Building blocks for a GHCI-like REPL with colon-commands.
module Climb
  ( Command
  , CommandErr (..)
  , Completion
  , OptionCommands
  , ReplDef (..)
  , ReplDirective (..)
  , bareCommand
  , noOptionCommands
  , noCompletion
  , runReplDef
  , stepReplDef
  ) where

import Control.Exception (Exception (..), SomeAsyncException (..), SomeException)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow (..), catchIf)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Linenoise.Repl (ReplDirective (..), replM)

-- | A 'Command' takes some input, performs some effect, and returns a directive (continue or quit).
type Command m = Text -> m ReplDirective

-- | List of 'Command's by name with help text.
type OptionCommands m = Map Text (Text, Command m)

-- | A 'Completion' takes some input and returns potential matches.
type Completion m = Text -> m [Text]

-- | Sometimes things go wrong...
data CommandErr
  = CommandErrExpectedNoInput
  -- ^ An option 'Command' got input when it expected None
  | CommandErrUnknownCommand !Text
  -- ^ An option 'Command' was not found by name.
  deriving stock (Eq, Show)

instance Exception CommandErr

-- | Defines a REPL with commands, options, and completion.
data ReplDef m =
  ReplDef
    { rdOnInterrupt :: !ReplDirective
    , rdGreeting :: !Text
    , rdPrompt :: !Text
    , rdOptionCommands :: !(OptionCommands m)
    , rdExecCommand :: !(Command m)
    , rdCompletion :: !(Completion m)
    }

noOptionCommands :: OptionCommands m
noOptionCommands = Map.empty

noCompletion :: Applicative m => Completion m
noCompletion = const (pure [])

assertEmpty :: MonadThrow m => Text -> m ()
assertEmpty input = unless (Text.null input) (throwM CommandErrExpectedNoInput)

-- | Helps you define commands that expect no input.
bareCommand :: MonadThrow m => m ReplDirective -> Command m
bareCommand act input = assertEmpty input >> act

quitCommand :: MonadThrow m => Command m
quitCommand = bareCommand (pure ReplQuit)

helpCommand :: (MonadThrow m, MonadIO m) => OptionCommands m -> Command m
helpCommand opts = bareCommand $ do
  liftIO (TIO.putStrLn "Available commands:")
  for_ (Map.toList opts) $ \(name, (desc, _)) -> liftIO (TIO.putStrLn (":" <> name <> "\t" <> desc))
  pure ReplContinue

defaultOptions :: (MonadThrow m, MonadIO m) => OptionCommands m -> OptionCommands m
defaultOptions opts = Map.fromList
    [ ("quit", ("quit", quitCommand))
    , ("help", ("describe all commands", helpCommand opts))
    ]

outerCommand :: MonadThrow m => OptionCommands m -> Command m -> Command m
outerCommand opts exec input =
  case Text.uncons input of
    Just (':', rest) -> do
      let (name, subInput) = Text.break (==' ') rest
      case Map.lookup name opts of
        Nothing -> throwM (CommandErrUnknownCommand name)
        Just (_, command) -> command (Text.drop 1 subInput)
    _ -> exec input

isUserErr :: SomeException -> Bool
isUserErr x =
  case fromException x of
    Just (SomeAsyncException _) -> False
    _ -> True

catchUserErr :: MonadCatch m => m a -> (SomeException -> m a) -> m a
catchUserErr = catchIf isUserErr

handleUserErr :: (MonadCatch m, MonadIO m) => Command m -> Command m
handleUserErr action input = catchUserErr (action input) $ \err -> do
  liftIO (TIO.putStr "Caught error: ")
  liftIO (print err)
  pure ReplContinue

-- | Runs a REPL as defined.
runReplDef :: (MonadCatch m, MonadUnliftIO m) => ReplDef m -> m ()
runReplDef (ReplDef onInterrupt greeting prompt opts exec comp) = do
  let allOpts = fix (\c -> defaultOptions c <> opts)
      action = outerCommand allOpts exec
      handledAction = handleUserErr action
  liftIO (TIO.putStrLn greeting)
  liftIO (TIO.putStrLn "Enter `:quit` to exit or `:help` to see all commands.")
  replM onInterrupt prompt handledAction comp

-- | Processes a single line of input. Useful for testing.
-- (Note that this does not handle default option commands.)
stepReplDef :: MonadThrow m => ReplDef m -> Text -> m ReplDirective
stepReplDef (ReplDef _ _ _ opts exec _) = outerCommand opts exec
