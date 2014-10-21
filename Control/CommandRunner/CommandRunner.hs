{- |
TODO bureaucratic stuff

Module
Description
Copyright
Licence

Maintainer
Stability
Portability
-}

module CommandRunner (

    CommandRunner
  , newCommandRunner
  , exec
  , execAppend
  , undo
  , redo
  , peek

  , Command
  , benign
  , modify
  , check

  ) where

-- We'll start with a thread-unsafe variant, and then make it thread safe
-- later!
import Data.IORef
import Control.Applicative
import Data.Monoid

-- | A Command_ is a function from some mutable environment to an IO.
newtype Command_ env a = Command_ (IORef env -> IO a)

runCommand_ :: Command_ env a -> IORef env -> IO a
runCommand_ (Command_ f) = f

instance Functor (Command_ e) where
  fmap f (Command_ g) = Command_ $ (fmap . fmap) f g

instance Applicative (Command_ e) where
  pure x = Command_ $ (pure . pure) x
  (Command_ f) <*> (Command_ g) = Command_ $ \y -> do
      h <- f y
      -- ^ Get the function from the first command.
      x <- g y
      -- ^ Get the point from the second command.
      return $ h x
      -- ^ return the pure computation

instance Monad (Command_ e) where
  return x = Command_ $ (return . return) x
  (Command_ f) >>= k = Command_ $ \y -> do
      x <- f y
      runCommand_ (k x) $ y

-- We want to say that (Command env) is a monad and that these are the
-- only injections:
benign :: a -> Command_ env a
benign = return

modify :: (env -> env) -> Command_ env ()
modify f = Command_ $ (flip modifyIORef) f

check :: Command_ env env
check = Command_ $ readIORef

-- | But any well-behaved Command env has to output another Command env with
--   value type Command env with value type Command evn and so on and so on.
newtype Command env = Command (Command_ env (Command env))

runCommand :: Command env -> IORef env -> IO (Command env)
runCommand (Command cmd) = runCommand_ cmd

extractCommand :: Command env -> Command_ env (Command env)
extractCommand (Command cmd) = cmd

-- | We compose Commands monoidally, not monadically. Commands aren't even
--   Functors!
instance Monoid (Command env) where
  -- mempty does nothing. To undo it is to do nothing.
  mempty = Command $ return mempty
  -- mappend sequences two Command actions, and returns a Command which runs
  -- their undos in reverse order.
  x `mappend` y = Command $ do
      undoX <- extractCommand x
      undoY <- extractCommand y
      return $ undoY `mappend` undoX

data CommandRunner env = CommandRunner {
    _environment :: IORef env
  , _undoStack :: IORef [Command env]
  , _redoStack :: IORef [Command env]
  }

cmdRunnerEnv :: CommandRunner env -> IORef env
cmdRunnerEnv (CommandRunner e _ _) = e

cmdRunnerUndoStack :: CommandRunner env -> IORef [Command env]
cmdRunnerUndoStack (CommandRunner _ u _) = u

cmdRunnerRedoStack :: CommandRunner env -> IORef [Command env]
cmdRunnerRedoStack (CommandRunner _ _ r) = r

-- The following functions are the entire interface.

-- | Give the initial environment to get a CommandRunner.
newCommandRunner :: env -> IO (CommandRunner env)
newCommandRunner e = do
  envIORef <- newIORef e
  undos <- newIORef []
  redos <- newIORef []
  return $ CommandRunner envIORef undos redos

exec :: CommandRunner env -> (env -> IO ()) -> Command env -> IO ()
exec cmdRunner k cmd = do
  undoCmd <- runCommand cmd (cmdRunnerEnv cmdRunner)
  modifyIORef (cmdRunnerUndoStack cmdRunner) ((:) undoCmd)
  modifyIORef (cmdRunnerRedoStack cmdRunner) (const [])
  currentEnvironment <- readIORef (cmdRunnerEnv cmdRunner)
  k currentEnvironment

execAppend :: CommandRunner env -> (env -> IO ()) -> Command env -> IO ()
execAppend cmdRunner k cmd = do
  undoCmd <- runCommand cmd (cmdRunnerEnv cmdRunner)
  modifyIORef (cmdRunnerUndoStack cmdRunner) (updateTopOfStack undoCmd)
  -- ^ The difference between execAppend and exec; we don't always push a new
  -- stack frame. Instead, if the stack is not empty, we append to the top of
  -- it, so that the undo frame includes this one.
  modifyIORef (cmdRunnerRedoStack cmdRunner) (const [])
  currentEnvironment <- readIORef (cmdRunnerEnv cmdRunner)
  k currentEnvironment
    where updateTopOfStack undoCmd [] = [undoCmd]
          updateTopOfStack undoCmd (x : xs) = (undoCmd `mappend` x) : xs
          -- ^ We append in front, so that this undo is run before the
          -- remaining undos!

undo :: CommandRunner env -> (env -> IO ()) -> IO ()
undo cmdRunner k = do
  topOfStack <- readIORef (cmdRunnerUndoStack cmdRunner)
  case topOfStack of
    [] -> return ()
    u : us -> do 
        redoCmd <- runCommand u (cmdRunnerEnv cmdRunner)
        modifyIORef (cmdRunnerUndoStack cmdRunner) (const us)
        modifyIORef (cmdRunnerRedoStack cmdRunner) ((:) redoCmd)
        currentEnvironment <- readIORef (cmdRunnerEnv cmdRunner)
        k currentEnvironment

redo :: CommandRunner env -> (env -> IO ()) -> IO ()
redo cmdRunner k = do
  topOfStack <- readIORef (cmdRunnerRedoStack cmdRunner)
  case topOfStack of
    [] -> return ()
    r : rs -> do
        undoCmd <- runCommand r (cmdRunnerEnv cmdRunner)
        modifyIORef (cmdRunnerRedoStack cmdRunner) (const rs)
        modifyIORef (cmdRunnerUndoStack cmdRunner) ((:) undoCmd)
        currentEnvironment <- readIORef (cmdRunnerEnv cmdRunner)
        k currentEnvironment

peek :: CommandRunner env -> IO env
peek cmdRunner = readIORef (cmdRunnerEnv cmdRunner)
