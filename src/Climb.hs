{-# LANGUAGE OverloadedStrings #-}

-- | Building blocks for a GHCI-like REPL with colon-commands.
module Climb
  ( Command
  , CommandErr (..)
  , Completion
  , OptionCommands
  , ReplDef (..)
  , ReplDirective (..)
  , TerminalWrite
  , bareCommand
  , noOptionCommands
  , noCompletion
  , runReplDef
  , runReplDefWithOutput
  , stepReplDef
  )
where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (Exception (..), SomeAsyncException (..), SomeException)
import qualified Control.Exception as Exception
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow (..), catchIf)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Linenoise.Repl (ReplDirective (..))
import qualified Linenoise.Unlift as Linenoise
import System.IO (hFlush, stdout)

-- | A 'Command' takes some input, performs some effect, and returns a directive (continue or quit).
type Command m = Text -> m ReplDirective

-- | List of 'Command's by name with help text.
type OptionCommands m = Map Text (Text, Command m)

-- | A 'Completion' takes some input and returns potential matches.
type Completion m = Text -> m [Text]

-- | A prompt-safe terminal writer.
--
-- Text written through this function is serialized with other writes. If a
-- linenoise prompt is active, the prompt and edit buffer are hidden before the
-- text is written and redrawn afterward. Include @"\n"@ when line-oriented
-- output is desired.
type TerminalWrite m = Text -> m ()

-- | Sometimes things go wrong...
data CommandErr
  = -- | An option 'Command' got input when it expected None
    CommandErrExpectedNoInput
  | -- | An option 'Command' was not found by name.
    CommandErrUnknownCommand !Text
  deriving stock (Eq, Show)

instance Exception CommandErr

-- | Defines a REPL with commands, options, and completion.
data ReplDef m = ReplDef
  { rdOnInterrupt :: !(m ReplDirective)
  -- ^ Action to run when the user interrupts the current prompt.
  , rdOnEof :: !(m ReplDirective)
  -- ^ Action to run when the user sends EOF.
  , rdGreeting :: !Text
  -- ^ Greeting printed before the first prompt.
  , rdPrompt :: !Text
  -- ^ Prompt text shown for each input line.
  , rdOptionCommands :: !(OptionCommands m)
  -- ^ Colon-prefixed option commands, keyed without the leading colon.
  , rdExecCommand :: !(Command m)
  -- ^ Command to run for non-option input.
  , rdCompletion :: !(Completion m)
  -- ^ Completion function for the current input line.
  }

-- | Empty option-command map.
noOptionCommands :: OptionCommands m
noOptionCommands = Map.empty

-- | Completion function that never returns candidates.
noCompletion :: (Applicative m) => Completion m
noCompletion = const (pure [])

assertEmpty :: (MonadThrow m) => Text -> m ()
assertEmpty input = unless (Text.null input) (throwM CommandErrExpectedNoInput)

-- | Helps you define commands that expect no input.
bareCommand :: (MonadThrow m) => m ReplDirective -> Command m
bareCommand act input = assertEmpty input >> act

quitCommand :: (MonadThrow m) => Command m
quitCommand = bareCommand (pure ReplQuit)

helpCommand :: (MonadThrow m) => TerminalWrite m -> OptionCommands m -> Command m
helpCommand write opts = bareCommand $ do
  writeLine write "Available commands:"
  for_ (Map.toList opts) $ \(name, (desc, _)) -> writeLine write (":" <> name <> "\t" <> desc)
  pure ReplContinue

defaultOptions :: (MonadThrow m) => TerminalWrite m -> OptionCommands m -> OptionCommands m
defaultOptions write opts =
  Map.fromList
    [ ("quit", ("quit", quitCommand))
    , ("help", ("describe all commands", helpCommand write opts))
    ]

outerCommand :: (MonadThrow m) => OptionCommands m -> Command m -> Command m
outerCommand opts exec input =
  case Text.uncons input of
    Just (':', rest) -> do
      let (name, subInput) = Text.break (== ' ') rest
      case Map.lookup name opts of
        Nothing -> throwM (CommandErrUnknownCommand name)
        Just (_, command) -> command (Text.drop 1 subInput)
    _ -> exec input

isUserErr :: SomeException -> Bool
isUserErr x =
  case fromException x of
    Just (SomeAsyncException _) -> False
    _ -> True

catchUserErr :: (MonadCatch m) => m a -> (SomeException -> m a) -> m a
catchUserErr = catchIf isUserErr

handleUserErr :: (MonadCatch m) => TerminalWrite m -> Command m -> Command m
handleUserErr write action input = catchUserErr (action input) $ \err -> do
  writeLine write ("Caught error: " <> Text.pack (show err))
  pure ReplContinue

-- | Runs a REPL as defined.
--
-- Use 'runReplDefWithOutput' when commands or background threads need a
-- prompt-safe 'TerminalWrite'.
runReplDef :: (MonadCatch m, MonadUnliftIO m) => ReplDef m -> m ()
runReplDef def = runReplDefWithOutput (const def)

-- | Runs a REPL built with prompt-safe terminal writers.
--
-- The builder receives a 'TerminalWrite' that can be captured by commands or
-- forked threads. All uses of that writer are serialized and integrated with
-- the active linenoise prompt so asynchronous output does not corrupt the
-- line the user is editing.
runReplDefWithOutput :: (MonadCatch m, MonadUnliftIO m) => (TerminalWrite m -> ReplDef m) -> m ()
runReplDefWithOutput buildDef = do
  currentSession <- liftIO (newIORef Nothing)
  outputLock <- liftIO (newMVar ())
  let write = writeText currentSession outputLock
      ReplDef onInterrupt onEof greeting prompt opts exec comp = buildDef write
      allOpts = fix (\c -> defaultOptions write c <> opts)
      action = outerCommand allOpts exec
      handledAction = handleUserErr write action
  writeLine write greeting
  writeLine write "Enter `:quit` to exit or `:help` to see all commands."
  Linenoise.setCompletion comp
  loop currentSession prompt onInterrupt onEof handledAction

loop
  :: (MonadUnliftIO m)
  => IORef (Maybe Linenoise.EditSession)
  -> Text
  -> m ReplDirective
  -> m ReplDirective
  -> Command m
  -> m ()
loop currentSession prompt onInterrupt onEof action = do
  res <- readLine currentSession prompt
  directive <- case res of
    Linenoise.InterruptResult -> onInterrupt
    Linenoise.EofResult -> onEof
    Linenoise.LineResult line -> do
      directive <- action line
      Linenoise.addHistory line
      pure directive
  case directive of
    ReplContinue -> loop currentSession prompt onInterrupt onEof action
    ReplQuit -> pure ()

readLine :: (MonadUnliftIO m) => IORef (Maybe Linenoise.EditSession) -> Text -> m (Linenoise.InputResult Text)
readLine currentSession prompt =
  withRunInIO $ \runInIO ->
    Linenoise.withEditSession prompt $ \session -> do
      writeIORef currentSession (Just session)
      runInIO (feed session) `Exception.finally` writeIORef currentSession Nothing

feed :: (MonadIO m) => Linenoise.EditSession -> m (Linenoise.InputResult Text)
feed session = do
  res <- Linenoise.feedEditSession session
  case res of
    Linenoise.MoreResult -> feed session
    Linenoise.DoneResult input -> pure input

writeText :: (MonadIO m) => IORef (Maybe Linenoise.EditSession) -> MVar () -> TerminalWrite m
writeText currentSession outputLock text =
  liftIO (writeSafely currentSession outputLock (TIO.putStr text >> hFlush stdout))

writeLine :: TerminalWrite m -> Text -> m ()
writeLine write text = write (text <> "\n")

writeSafely :: IORef (Maybe Linenoise.EditSession) -> MVar () -> IO () -> IO ()
writeSafely currentSession outputLock action =
  withMVar outputLock $ \_ -> do
    session <- readIORef currentSession
    case session of
      Nothing -> action
      Just session' -> Linenoise.withHiddenEditSession session' action

-- | Processes a single line of input. Useful for testing.
-- (Note that this does not handle default option commands.)
stepReplDef :: (MonadThrow m) => ReplDef m -> Text -> m ReplDirective
stepReplDef (ReplDef _ _ _ _ opts exec _) = outerCommand opts exec
