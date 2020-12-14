module Days.Day14 where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Bits
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Program.RunDay as R (runDay)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  let maskParser = do
        _ <- string "mask = "
        val <- takeTill isEndOfLine
        return $
          foldr
            ( \(c, i) m -> case c of
                '0' -> Map.insert i 0 m
                '1' -> Map.insert i 1 m
                _ -> m
            )
            Map.empty
            (zip (T.unpack val) (reverse [0 .. 35]))
  let memParser = do
        _ <- string "mem["
        adr <- decimal
        _ <- string "] = "
        Mem adr <$> decimal
  let lineParser = (Mask <$> maskParser) <|> memParser
  initialMask <- maskParser
  _ <- endOfLine
  rest <- lineParser `sepBy` endOfLine
  return $ InstructionList initialMask rest

------------ TYPES ------------
type MaskT = Map.Map Int Int

data Instruction = Mask MaskT | Mem Int Int deriving (Show)

data InstructionList = InstructionList MaskT [Instruction]
  deriving (Show)

type Input = InstructionList

------------ PART A ------------
partA :: Input -> Int
partA (InstructionList initial rest) = go Map.empty initial rest
  where
    applyMask mask val =
      Map.foldrWithKey (\k a b -> clearBit b k .|. (a `shiftL` k)) val mask
    go mem _ [] = sum mem
    go mem mask (x : xs) = case x of
      Mask newMask -> go mem newMask xs
      Mem adr val -> go (Map.insert adr (applyMask mask val) mem) mask xs

------------ PART B ------------
partB :: Input -> Int
partB = error "Not implemented yet!"
