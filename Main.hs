module Main (main) where

import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Control.Monad.Trans.State.Strict qualified as State
import Data.Foldable (fold)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Optics
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Exit (exitFailure)
import System.Random qualified as Random
import Termbox.Banana (Key (..), Pos (..), bg, blue, fg, green, posDown, posRight, red, yellow)
import Termbox.Banana qualified as Term

main :: IO ()
main = do
  initialGame <- randomGame
  Term.run (main1 initialGame) >>= \case
    Left _err -> exitFailure
    Right () -> pure ()

main1 :: Game -> Term.Inputs -> MomentIO (Term.Outputs ())
main1 initialGame Term.Inputs {keys} = do
  let eMove =
        filterJust
          ( keys <&> \case
              KeyArrowDown -> Just D
              KeyArrowLeft -> Just L
              KeyArrowRight -> Just R
              KeyArrowUp -> Just U
              _ -> Nothing
          )
  bGuy <-
    stepper
      BlueGuy
      ( filterJust
          ( keys <&> \case
              KeyChar '1' -> Just BlueGuy
              KeyChar '2' -> Just GreenGuy
              KeyChar '3' -> Just RedGuy
              KeyChar '4' -> Just YellowGuy
              _ -> Nothing
          )
      )
  bGame <-
    accumB
      initialGame
      ( ( \guy ->
            case guy of
              BlueGuy -> moveBlueGuy
              GreenGuy -> moveGreenGuy
              RedGuy -> moveRedGuy
              YellowGuy -> moveYellowGuy
        )
          <$> bGuy <@> eMove
      )
  pure
    Term.Outputs
      { scene = irender (Pos 0 0) . render <$> bGame,
        done = () <$ filterE (== KeyEsc) keys
      }

data Dir = D | L | R | U

data Guy = BlueGuy | GreenGuy | RedGuy | YellowGuy

data Game = Game
  { origBlueGuy :: !Pos,
    origGreenGuy :: !Pos,
    origRedGuy :: !Pos,
    origYellowGuy :: !Pos,
    blueGuy :: !Pos,
    greenGuy :: !Pos,
    redGuy :: !Pos,
    yellowGuy :: !Pos,
    moves :: !Int,
    horizontalWalls :: !(Vector IntSet),
    verticalWalls :: !(Vector IntSet)
  }
  deriving stock (Generic)

randomGame :: IO Game
randomGame = do
  horizontalWalls <- pure initialHorizontalWalls
  verticalWalls <- pure initialVerticalWalls
  let maxRow = Vector.length horizontalWalls - 1
  let maxCol = Vector.length verticalWalls - 1
  let initialOccupied =
        Set.fromList do
          row <- [vlenDivTwo, vlenDivTwo + 1]
          col <- [hlenDivTwo, hlenDivTwo + 1]
          pure (Pos row col)
        where
          hlenDivTwo = Vector.length horizontalWalls `div` 2
          vlenDivTwo = Vector.length verticalWalls `div` 2
  (blueGuy, greenGuy, redGuy, yellowGuy) <-
    flip evalStateT initialOccupied do
      (,,,)
        <$> randomPosition maxRow maxCol
        <*> randomPosition maxRow maxCol
        <*> randomPosition maxRow maxCol
        <*> randomPosition maxRow maxCol
  pure
    Game
      { origBlueGuy = blueGuy,
        origGreenGuy = greenGuy,
        origRedGuy = redGuy,
        origYellowGuy = yellowGuy,
        blueGuy,
        greenGuy,
        redGuy,
        yellowGuy,
        moves = 0,
        horizontalWalls = initialHorizontalWalls,
        verticalWalls = initialVerticalWalls
      }

randomPosition :: Int -> Int -> StateT (Set Pos) IO Pos
randomPosition maxRow maxCol = do
  occupied <- State.get
  let loop = do
        pos <- liftIO (Pos <$> Random.randomRIO (0, maxRow) <*> Random.randomRIO (0, maxCol))
        if Set.member pos occupied then loop else pure pos
  pos <- loop
  State.put $! Set.insert pos occupied
  pure pos

initialHorizontalWalls :: Vector IntSet
initialHorizontalWalls =
  (Vector.fromList . map IntSet.fromList)
    [ [5, 10, 15],
      [1, 12, 15],
      [15],
      [6, 14, 15],
      [9, 15],
      [15],
      [1, 4, 9, 15],
      [6, 8, 11, 15],
      [6, 8, 10, 15],
      [1, 13, 15],
      [4, 15],
      [15],
      [5, 15],
      [10, 15],
      [1, 13, 15],
      [4, 9, 15]
    ]

initialVerticalWalls :: Vector IntSet
initialVerticalWalls =
  (Vector.fromList . map IntSet.fromList)
    [ [4, 10, 15],
      [6, 8, 15],
      [0, 14, 15],
      [15],
      [10, 15],
      [6, 15],
      [2, 11, 15],
      [6, 8, 15],
      [6, 8, 15],
      [3, 15],
      [5, 8, 15],
      [12, 15],
      [7, 15],
      [1, 8, 15],
      [3, 14, 15],
      [4, 11, 15]
    ]

moveBlueGuy :: Dir -> Game -> Game
moveBlueGuy dir game =
  moveGuy #blueGuy (game ^. #greenGuy) (game ^. #redGuy) (game ^. #yellowGuy) dir game

moveGreenGuy :: Dir -> Game -> Game
moveGreenGuy dir game =
  moveGuy #greenGuy (game ^. #blueGuy) (game ^. #redGuy) (game ^. #yellowGuy) dir game

moveRedGuy :: Dir -> Game -> Game
moveRedGuy dir game =
  moveGuy #redGuy (game ^. #blueGuy) (game ^. #greenGuy) (game ^. #yellowGuy) dir game

moveYellowGuy :: Dir -> Game -> Game
moveYellowGuy dir game =
  moveGuy #yellowGuy (game ^. #blueGuy) (game ^. #greenGuy) (game ^. #redGuy) dir game

moveGuy :: Lens' Game Pos -> Pos -> Pos -> Pos -> Dir -> Game -> Game
moveGuy guy1 guy2 guy3 guy4 dir game =
  case dir of
    D ->
      case IntSet.lookupGE guy1r horizontalStoppers of
        Just row
          | row /= guy1r ->
              game
                & (guy1 % #row .~ row)
                & (#moves %~ (+ 1))
        _ -> game
    L ->
      let col = maybe 0 (+ 1) (IntSet.lookupLT guy1c verticalStoppers)
       in if col /= guy1c
            then
              game
                & (guy1 % #col .~ col)
                & (#moves %~ (+ 1))
            else game
    R ->
      case IntSet.lookupGE guy1c verticalStoppers of
        Just col
          | guy1c /= col ->
              game
                & (guy1 % #col .~ col)
                & (#moves %~ (+ 1))
        _ -> game
    U ->
      let row = maybe 0 (+ 1) (IntSet.lookupLT (game ^. guy1 % #row) horizontalStoppers)
       in if row /= guy1r
            then
              game
                & (guy1 % #row .~ row)
                & (#moves %~ (+ 1))
            else game
  where
    guy1c = game ^. guy1 % #col
    guy1r = game ^. guy1 % #row

    horizontalStoppers =
      ((game ^. #horizontalWalls) Vector.! (game ^. guy1 % #col))
        & (if guy2 ^. #col == game ^. guy1 % #col then IntSet.insert (guy2 ^. #row) . IntSet.insert (guy2 ^. #row - 1) else id)
        & (if guy3 ^. #col == game ^. guy1 % #col then IntSet.insert (guy3 ^. #row) . IntSet.insert (guy3 ^. #row - 1) else id)
        & (if guy4 ^. #col == game ^. guy1 % #col then IntSet.insert (guy4 ^. #row) . IntSet.insert (guy4 ^. #row - 1) else id)
    verticalStoppers =
      ((game ^. #verticalWalls) Vector.! (game ^. guy1 % #row))
        & (if guy2 ^. #row == game ^. guy1 % #row then IntSet.insert (guy2 ^. #col) . IntSet.insert (guy2 ^. #col - 1) else id)
        & (if guy3 ^. #row == game ^. guy1 % #row then IntSet.insert (guy3 ^. #col) . IntSet.insert (guy3 ^. #col - 1) else id)
        & (if guy4 ^. #row == game ^. guy1 % #row then IntSet.insert (guy4 ^. #col) . IntSet.insert (guy4 ^. #col - 1) else id)

------------------------------------------------------------------------------------------------------------------------
-- Rendering

--     0   1   2   3
--   +---+---+---+---+
-- 0 |   |   |   |   |
--   +---+---+---+---+
-- 1 |   | X |   |   |
--   +---+---+---+---+
-- 2 |   |   |   |   |
--   +---+---+---+---+
-- 3 |   |   |   |   |
--   +---+---+---+---+

render :: Game -> Image
render game@Game {moves, horizontalWalls, verticalWalls} =
  ivcat
    [ ihcat [istring "Moves: ", istring (show moves)],
      fold
        [ renderHorizontalWalls horizontalWalls,
          renderVerticalWalls verticalWalls,
          renderIntersections horizontalWalls verticalWalls,
          renderOrigGuy (game ^. #origBlueGuy) blue,
          renderOrigGuy (game ^. #origGreenGuy) green,
          renderOrigGuy (game ^. #origRedGuy) red,
          renderOrigGuy (game ^. #origYellowGuy) yellow,
          renderGuy (game ^. #blueGuy) blue,
          renderGuy (game ^. #greenGuy) green,
          renderGuy (game ^. #redGuy) red,
          renderGuy (game ^. #yellowGuy) yellow
        ]
    ]

renderHorizontalWalls :: Vector IntSet -> Image
renderHorizontalWalls walls =
  [0 .. (Vector.length walls - 1)] & foldMap \col ->
    IntSet.foldl'
      (\acc row -> acc <> wall (Pos (row + 1) col))
      (wall (Pos 0 col))
      (walls Vector.! col)
  where
    wall :: Pos -> Image
    wall (Pos row col) =
      [1, 2, 3] & foldMap \offset ->
        ipos (Pos (2 * row) (col * 4 + offset)) hwall

renderVerticalWalls :: Vector IntSet -> Image
renderVerticalWalls walls =
  [0 .. (Vector.length walls - 1)] & foldMap \row ->
    IntSet.foldl'
      (\acc col -> acc <> wall (Pos row (col + 1)))
      (wall (Pos row 0))
      (walls Vector.! row)
  where
    wall :: Pos -> Image
    wall (Pos row col) =
      ipos (Pos (row * 2 + 1) (col * 4)) vwall

renderIntersections :: Vector IntSet -> Vector IntSet -> Image
renderIntersections horizontalWalls verticalWalls =
  [0 .. hlen] & foldMap \col ->
    [0 .. vlen] & foldMap \row ->
      let pos = Pos (row * 2) (col * 4)
          wallLeft = col >= 1 && (row == 0 || IntSet.member (row - 1) (horizontalWalls Vector.! (col - 1)))
          wallRight = col < hlen && (row == 0 || IntSet.member (row - 1) (horizontalWalls Vector.! col))
          wallAbove = row >= 1 && (col == 0 || IntSet.member (col - 1) (verticalWalls Vector.! (row - 1)))
          wallBelow = row < vlen && (col == 0 || IntSet.member (col - 1) (verticalWalls Vector.! row))
       in case (wallLeft, wallRight, wallAbove, wallBelow) of
            (False, False, True, True) -> ipos pos vwall
            (False, True, False, True) -> ipos pos (ichar '┏')
            (False, True, True, False) -> ipos pos (ichar '┗')
            (False, True, True, True) -> ipos pos (ichar '┣')
            (True, False, False, True) -> ipos pos (ichar '┓')
            (True, False, True, False) -> ipos pos (ichar '┛')
            (True, False, True, True) -> ipos pos (ichar '┫')
            (True, True, False, False) -> ipos pos hwall
            (True, True, False, True) -> ipos pos (ichar '┳')
            (True, True, True, False) -> ipos pos (ichar '┻')
            (True, True, True, True) -> ipos pos (ichar '╋')
            _ -> mempty
  where
    hlen = Vector.length horizontalWalls
    vlen = Vector.length verticalWalls

renderOrigGuy :: Pos -> Term.Color -> Image
renderOrigGuy (Pos row col) color =
  ichar '░'
    & ifg color
    & ipos (Pos (row * 2 + 1) (col * 4 + 2))

renderGuy :: Pos -> Term.Color -> Image
renderGuy (Pos row col) color =
  ichar ' '
    & ibg color
    & ipos (Pos (row * 2 + 1) (col * 4 + 2))

hwall :: Image
hwall = ichar '━'

vwall :: Image
vwall = ichar '┃'

------------------------------------------------------------------------------------------------------------------------
-- Rendering toolkit

data M = M !Term.Scene !Int !Int

instance Monoid M where
  mappend = (<>)
  mempty = M mempty 0 0

instance Semigroup M where
  M s1 r1 c1 <> M s2 r2 c2 =
    M (s1 <> s2) (max r1 r2) (max c1 c2)

data Image
  = ImageCell !Term.Cell
  | ImageH !Image !Image
  | ImageV !Image !Image
  | ImageA !Image !Image
  | ImageFg !Term.Color !Image
  | ImageBg !Term.Color !Image
  | ImagePos !Pos !Image
  | ImageEmpty

instance Monoid Image where
  mappend = (<>)
  mempty = ImageEmpty

instance Semigroup Image where
  (<>) = ImageA

ichar :: Char -> Image
ichar c = ImageCell (Term.char c)

istring :: [Char] -> Image
istring =
  ihcat . map ichar

ihcat :: [Image] -> Image
ihcat =
  foldr ImageH ImageEmpty

ivcat :: [Image] -> Image
ivcat =
  foldr ImageV ImageEmpty

ifg :: Term.Color -> Image -> Image
ifg = ImageFg

ibg :: Term.Color -> Image -> Image
ibg = ImageBg

ipos :: Pos -> Image -> Image
ipos = ImagePos

irender :: Pos -> Image -> Term.Scene
irender pos img =
  let M s _ _ = irenderm pos img in s

irenderm :: Pos -> Image -> M
irenderm =
  go id id
  where
    go addFg addBg pos = \case
      ImageCell c -> M (Term.cell pos (addBg (addFg c))) 1 1
      ImageH i1 i2 ->
        let M s1 r1 c1 = go addFg addBg pos i1
            M s2 r2 c2 = go addFg addBg (posRight c1 pos) i2
         in M (s1 <> s2) (max r1 r2) (c1 + c2)
      ImageV i1 i2 ->
        let M s1 r1 c1 = go addFg addBg pos i1
            M s2 r2 c2 = go addFg addBg (posDown r1 pos) i2
         in M (s1 <> s2) (r1 + r2) (max c1 c2)
      ImageA i1 i2 ->
        let M s1 r1 c1 = go addFg addBg pos i1
            M s2 r2 c2 = go addFg addBg pos i2
         in M (s1 <> s2) (max r1 r2) (max c1 c2)
      ImageFg c i -> go (fg c) addBg pos i
      ImageBg c i -> go addFg (bg c) pos i
      ImagePos p i -> go addFg addBg (translate pos p) i
      ImageEmpty -> mempty

translate :: Pos -> Pos -> Pos
translate (Pos r1 c1) (Pos r2 c2) = Pos (r1 + r2) (c1 + c2)
