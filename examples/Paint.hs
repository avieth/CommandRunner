import qualified Data.Set as S
import qualified Data.Map as M
import Data.Ratio

-- Random point.
import System.Random
import Debug.Trace

import Control.CommandRunner

data Colour = Red | Green | Blue
-- We use Ratio because we want reliable equality.
type Point = (Rational, Rational)
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

main = do
  cmdRunner <- newCommandRunner emptyDrawingState
  mainLoop cmdRunner

mainLoop cmdRunner = do
  putChar '>'
  putChar ' '
  c <- getChar
  putChar '\n'
  case c of
    'u' -> undo cmdRunner (const $ return ())
    'r' -> redo cmdRunner (const $ return ())
    'p' -> peek cmdRunner >>= print
    'n' -> do
        gen <- newStdGen
        let (randomId, gen1) = randomR (1, 100) gen
        let (randomNumerator1, gen2) = random gen1
        let (randomDenominator1, gen3) = random gen2
        let (randomNumerator2, gen4) = random gen3
        let (randomDenominator2, gen5) = random gen4
        let randomPoint = (randomNumerator1 % randomDenominator1, randomNumerator2 % randomDenominator2)
        print randomId
        print randomPoint
        exec cmdRunner (const $ return ()) (addPoint randomId randomPoint)
    _ -> return ()
  mainLoop cmdRunner
