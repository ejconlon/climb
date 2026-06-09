{-# LANGUAGE OverloadedStrings #-}

module Main where

import Climb
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (Exception)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Text.Read (readMaybe)

newtype BadGuessErr = BadGuessErr (Maybe Int)
  deriving stock (Eq, Show)

instance Exception BadGuessErr

newtype ReplEnv = ReplEnv
  { reMagicNumber :: Int
  }
  deriving stock (Eq, Show)

newtype ReplM a = ReplM {unReplM :: ReaderT ReplEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader ReplEnv, MonadIO, MonadUnliftIO, MonadThrow, MonadCatch)

runReplM :: ReplM a -> ReplEnv -> IO a
runReplM = runReaderT . unReplM

guessCommand :: TerminalWrite ReplM -> Command ReplM
guessCommand write input = do
  num <- asks reMagicNumber
  let mayGuess = readMaybe (T.unpack input)
  if readMaybe (T.unpack input) == Just num
    then ReplContinue <$ write "You guessed it!\n"
    else throwM (BadGuessErr mayGuess)

delayCommand :: TerminalWrite ReplM -> Command ReplM
delayCommand write _ = do
  env <- ask
  liftIO $ void $ forkIO $ do
    threadDelay 2000000
    runReplM (write "delayed log\n") env
  write "scheduled delayed log\n"
  pure ReplContinue

options :: TerminalWrite ReplM -> OptionCommands ReplM
options write =
  Map.fromList
    [ ("guess", ("guess a number", guessCommand write))
    , ("delay", ("log after 2 seconds", delayCommand write))
    ]

exec :: TerminalWrite ReplM -> Command ReplM
exec write input = do
  write ("You said: " <> input <> "\n")
  pure ReplContinue

completion :: Completion ReplM
completion _ = pure []

replDef :: TerminalWrite ReplM -> ReplDef ReplM
replDef write =
  ReplDef
    { rdOnInterrupt = pure ReplContinue
    , rdOnEof = pure ReplQuit
    , rdGreeting = "Hello, REPL!"
    , rdPrompt = "> "
    , rdOptionCommands = options write
    , rdExecCommand = exec write
    , rdCompletion = completion
    }

main :: IO ()
main = runReplM (runReplDefWithOutput replDef) (ReplEnv 42)
