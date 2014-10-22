-- It's a mess in here. We use SDL to make a silly drawing app. Click around
-- to add points, and use z and y to undo/redo, any other key will quit.
-- NB commands add _points_ not lines, so if you undo all the way, it will
-- seem like the first redo does nothing, but in fact it adds a point.

import Prelude hiding (init)

import qualified Data.Set as S
import qualified Data.Map as M
import Data.String
import Data.Traversable

import Graphics.UI.SDL.Basic
import Graphics.UI.SDL.Enum
import Graphics.UI.SDL.Types hiding (Point)
import Graphics.UI.SDL.Event
import Graphics.UI.SDL.Video

import Control.CommandRunner
import Control.Applicative
import Control.Monad

import Foreign.C.String
import qualified Foreign.Storable as FS
import Foreign.Marshal.Alloc

import Debug.Trace

data Colour = Red | Green | Blue
type Point = (Int, Int)
type PointListIdentifier = Int
type PointList = [Point]
type PointLists = M.Map PointListIdentifier PointList

-- | Values of this type completely describe what is drawn on screen.
data DrawingState = DrawingState {
    _pointLists :: PointLists
  } deriving (Show)

emptyDrawingState :: DrawingState
emptyDrawingState = DrawingState {
    _pointLists = M.empty
  }

maximumKey :: DrawingState -> PointListIdentifier
maximumKey = (M.foldrWithKey folder 0) . _pointLists
  where folder key _ maximal = max key maximal

addPoint' :: PointListIdentifier -> Point -> DrawingState -> DrawingState
addPoint' pid point ds = ds {
    _pointLists = M.alter alterer pid (_pointLists ds)
  }
  where alterer Nothing = Just [point]
        alterer (Just ps) = Just $ point : ps

removePoint' :: PointListIdentifier -> Point -> DrawingState -> DrawingState
removePoint' pid point ds = ds {
    _pointLists = M.update updater pid (_pointLists ds)
  }
  where updater ps = case (filter ((/=) point) ps) of
          [] -> Nothing
          ps' -> Just ps'

addPoint :: PointListIdentifier -> Point -> Command DrawingState
addPoint pid point = Command $ do
  modify (addPoint' pid point)
  -- We assume the identifier exists. TODO don't.
  return $ removePoint pid point

removePoint :: PointListIdentifier -> Point -> Command DrawingState
removePoint pid point = Command $ do
  modify (removePoint' pid point)
  return $ addPoint pid point

-- Here we see how Haskell shines as the world's best imperative programming
-- language.
main = do
  cmdRunner <- newCommandRunner emptyDrawingState
  status <- fromIntegral <$> init initFlagEverything
  unless (status == 0) (mainSDL cmdRunner)
  quit

mainSDL cmdRunner = do
  window <- withCString "Paint" (\s -> createWindow s (fromIntegral (0 :: Int)) (fromIntegral (0 :: Int)) (fromIntegral (640 :: Int)) (fromIntegral (480 :: Int)) windowFlagShown)
  renderer <- createRenderer window (fromIntegral ((-1) :: Int)) rendererFlagAccelerated
  mainLoop cmdRunner renderer
  destroyRenderer renderer
  destroyWindow window

mainLoop cmdRunner renderer = do
  -- Start by clearing
  setRenderDrawColor renderer (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 255)
  renderClear renderer
  -- Draw all lines
  state <- peek cmdRunner
  setRenderDrawColor renderer (fromIntegral 255) (fromIntegral 255) (fromIntegral 255) (fromIntegral 255)
  drawLines renderer state
  renderPresent renderer
  -- Wait for input.
  waitForHandledEvent cmdRunner renderer

waitForHandledEvent cmdRunner renderer = alloca $ \eventPtr -> do
  waitEvent eventPtr
  ev <- FS.peek eventPtr
  case ev of
    k@(KeyboardEvent _ _ _ _ _ _) -> handleKeyboardEvent cmdRunner renderer k
    m@(MouseButtonEvent _ _ _ _ _ _ _ _ _) -> handleMouseEvent cmdRunner renderer m
    _ -> waitForHandledEvent cmdRunner renderer

handleKeyboardEvent cmdRunner renderer kb = do
  let code = (keysymKeycode . keyboardEventKeysym) kb
  if code == keycodeZ
  then undo cmdRunner k
  else if code == keycodeY
    then redo cmdRunner k
    -- Any other key will quit!
    else return ()
  where k = \_ -> mainLoop cmdRunner renderer

handleMouseEvent cmdRunner renderer mb = do
  exec cmdRunner k (addPoint 0 (x, y))
    where x = fromIntegral $ mouseButtonEventX mb
          y = fromIntegral $ mouseButtonEventY mb
          k = \_ -> mainLoop cmdRunner renderer

drawLines renderer ds = traverse (drawPointList renderer) (_pointLists ds) >> return ()

drawPointList renderer pl = mapM_ (sdlRenderLine renderer) pairs
  where pairs = pl `zip` (tail pl)

sdlRenderLine renderer (p, q) = renderDrawLine renderer x0 y0 x1 y1 >> return ()
  where x0 = fromIntegral . fst $ p
        y0 = fromIntegral . snd $ p
        x1 = fromIntegral . fst $ q
        y1 = fromIntegral . snd $ q
