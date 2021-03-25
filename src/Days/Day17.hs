module Days.Day17 where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Program.RunDay as R (runDay)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

inputParser :: Parser Input
inputParser = do
  let parseActive = char '#' >> return Active
      parseInactive = char '.' >> return Inactive
      lineParser = do
        v <- many' (parseActive <|> parseInactive)
        return $ zip [0 ..] v
  l <- lineParser `sepBy` endOfLine
  let concatted =
        concat (zipWith (\y l' -> (\(x, s) -> ((x, y), s)) <$> l') [0 ..] l)
  return $ Set.fromList $ fst <$> filter (\(_, s) -> s == Active) concatted

neighbourRelInds :: [Int]
neighbourRelInds = [-1, 0, 1]

class Coordinate a where
  neighbourIndices :: [a]
  addIndex :: a -> a -> a

type Coordinate2D = (Int, Int)

type Coordinate3D = (Int, Int, Int)

type Coordinate4D = (Int, Int, Int, Int)

instance Coordinate Coordinate3D where
  neighbourIndices =
    [ (a, b, c)
      | a <- neighbourRelInds,
        b <- neighbourRelInds,
        c <- neighbourRelInds,
        a /= 0 || b /= 0 || c /= 0
    ]
  addIndex (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

instance Coordinate Coordinate4D where
  neighbourIndices =
    [ (a, b, c, d)
      | a <- neighbourRelInds,
        b <- neighbourRelInds,
        c <- neighbourRelInds,
        d <- neighbourRelInds,
        a /= 0 || b /= 0 || c /= 0 || d /= 0
    ]
  addIndex (x1, y1, z1, w1) (x2, y2, z2, w2) =
    (x1 + x2, y1 + y2, z1 + z2, w1 + w2)

data State = Active | Inactive
  deriving (Show, Eq)

type CoordinateMap c = Map.Map c State

type Input = Set.Set Coordinate2D

type OutputA = Int

type OutputB = Int

neighbours :: Coordinate c => c -> [c]
neighbours c = addIndex c <$> neighbourIndices

runCycle :: (Coordinate c, Ord c) => Set.Set c -> Set.Set c
runCycle activeCubes =
  let m =
        Set.foldr
          ( \a b ->
              foldr
                (\a' b' -> Map.insertWith (+) a' 1 b')
                b
                (neighbours a)
          )
          (Map.empty :: Map.Map c Int)
          activeCubes
   in Map.foldrWithKey
        ( \k a b ->
            let isActive = Set.member k activeCubes
             in if (isActive && (a == 2 || a == 3)) || (not isActive && a == 3)
                  then Set.insert k b
                  else b
        )
        Set.empty
        m

go :: (Coordinate c, Ord c) => Set.Set c -> Int -> Int
go m limit = go' m 0
  where
    go' m' cycle'
      | cycle' == limit = Set.size m'
      | otherwise = go' (runCycle m') (cycle' + 1)

partA :: Input -> OutputA
partA i = go as3D 6
  where
    (as3D :: Set.Set Coordinate3D) =
      Set.map (\(x, y) -> (x, y, 0)) i

partB :: Input -> OutputB
partB i = go as4D 6
  where
    (as4D :: Set.Set Coordinate4D) =
      Set.map (\(x, y) -> (x, y, 0, 0)) i
