{-# LANGUAGE OverloadedStrings #-}

module Main where

import Climb
import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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

guessCommand :: Command ReplM
guessCommand input = do
  num <- asks reMagicNumber
  let mayGuess = readMaybe (T.unpack input)
  if readMaybe (T.unpack input) == Just num
    then ReplContinue <$ liftIO (putStrLn "You guessed it!")
    else throwM (BadGuessErr mayGuess)

options :: OptionCommands ReplM
options = Map.fromList [("guess", ("guess a number", guessCommand))]

exec :: Command ReplM
exec input = liftIO $ do
  putStr "You said: "
  TIO.putStrLn input
  pure ReplContinue

completion :: Completion ReplM
completion _ = pure []

replDef :: ReplDef ReplM
replDef =
  ReplDef
    { rdOnInterrupt = ReplContinue
    , rdGreeting = "Hello, REPL!"
    , rdPrompt = "> "
    , rdOptionCommands = options
    , rdExecCommand = exec
    , rdCompletion = completion
    }

main :: IO ()
main = runReplM (runReplDef replDef) (ReplEnv 42)
