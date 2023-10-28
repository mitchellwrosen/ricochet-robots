module Main
  ( main,
  )
where

import Data.Foldable (fold)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Optics
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Exit (exitFailure)
import Termbox.Banana (Key (..), Pos (..), bg, blue, char, green, red, yellow)
import Termbox.Banana qualified as Term

main :: IO ()
main =
  Term.run main1 >>= \case
    Left _err -> exitFailure
    Right () -> pure ()

main1 :: Term.Inputs -> MomentIO (Term.Outputs ())
main1 Term.Inputs {keys} = do
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
      Game
        { blueGuy = Pos 0 0,
          greenGuy = Pos 1 0,
          redGuy = Pos 2 2,
          yellowGuy = Pos 3 3,
          horizontalWalls = initialHorizontalWalls,
          verticalWalls = initialVerticalWalls
        }
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
      { scene = renderSceneAt (Pos 0 0) . render <$> bGame,
        done = () <$ filterE (== KeyEsc) keys
      }

data Dir = D | L | R | U

data Guy = BlueGuy | GreenGuy | RedGuy | YellowGuy

data Game = Game
  { blueGuy :: !Pos,
    greenGuy :: !Pos,
    redGuy :: !Pos,
    yellowGuy :: !Pos,
    horizontalWalls :: !(Vector IntSet),
    verticalWalls :: !(Vector IntSet)
  }
  deriving stock (Generic)

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
      case IntSet.lookupGE (game ^. guy1 % #row) horizontalStoppers of
        Nothing -> game
        Just row -> game & guy1 % #row .~ row
    L -> game & guy1 % #col .~ (maybe 0 (+ 1) (IntSet.lookupLT (game ^. guy1 % #col) verticalStoppers))
    R ->
      case IntSet.lookupGE (game ^. guy1 % #col) verticalStoppers of
        Nothing -> game
        Just col -> game & guy1 % #col .~ col
    U -> game & guy1 % #row .~ (maybe 0 (+ 1) (IntSet.lookupLT (game ^. guy1 % #row) horizontalStoppers))
  where
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
-- 4 |   |   |   |   |
--   +---+---+---+---+

render :: Game -> Scene
render game@Game {horizontalWalls, verticalWalls} =
  fold
    [ renderHorizontalWalls horizontalWalls,
      renderVerticalWalls verticalWalls,
      renderIntersections horizontalWalls verticalWalls,
      renderGuy (game ^. #blueGuy) blue,
      renderGuy (game ^. #greenGuy) green,
      renderGuy (game ^. #redGuy) red,
      renderGuy (game ^. #yellowGuy) yellow
    ]

renderHorizontalWalls :: Vector IntSet -> Scene
renderHorizontalWalls walls =
  [0 .. (Vector.length walls - 1)] & foldMap \col ->
    IntSet.foldl'
      (\acc row -> acc <> wall (Pos (row + 1) col))
      (wall (Pos 0 col))
      (walls Vector.! col)
  where
    wall :: Pos -> Scene
    wall (Pos row col) =
      [1, 2, 3] & foldMap \offset ->
        cell (Pos (2 * row) (col * 4 + offset)) hwall

renderVerticalWalls :: Vector IntSet -> Scene
renderVerticalWalls walls =
  [0 .. (Vector.length walls - 1)] & foldMap \row ->
    IntSet.foldl'
      (\acc col -> acc <> wall (Pos row (col + 1)))
      (wall (Pos row 0))
      (walls Vector.! row)
  where
    wall :: Pos -> Scene
    wall (Pos row col) =
      cell (Pos (row * 2 + 1) (col * 4)) vwall

renderIntersections :: Vector IntSet -> Vector IntSet -> Scene
renderIntersections horizontalWalls verticalWalls =
  [0 .. hlen] & foldMap \col ->
    [0 .. vlen] & foldMap \row ->
      let pos = Pos (row * 2) (col * 4)
          wallLeft = col >= 1 && (row == 0 || IntSet.member (row - 1) (horizontalWalls Vector.! (col - 1)))
          wallRight = col < hlen && (row == 0 || IntSet.member (row - 1) (horizontalWalls Vector.! col))
          wallAbove = row >= 1 && (col == 0 || IntSet.member (col - 1) (verticalWalls Vector.! (row - 1)))
          wallBelow = row < vlen && (col == 0 || IntSet.member (col - 1) (verticalWalls Vector.! row))
       in case (wallLeft, wallRight, wallAbove, wallBelow) of
            (False, False, True, True) -> cell pos vwall
            (False, True, False, True) -> cell pos (char '┏')
            (False, True, True, False) -> cell pos (char '┗')
            (False, True, True, True) -> cell pos (char '┣')
            (True, False, False, True) -> cell pos (char '┓')
            (True, False, True, False) -> cell pos (char '┛')
            (True, False, True, True) -> cell pos (char '┫')
            (True, True, False, False) -> cell pos hwall
            (True, True, False, True) -> cell pos (char '┳')
            (True, True, True, False) -> cell pos (char '┻')
            (True, True, True, True) -> cell pos (char '╋')
            _ -> mempty
  where
    hlen = Vector.length horizontalWalls
    vlen = Vector.length verticalWalls

renderGuy :: Pos -> Term.Color -> Scene
renderGuy (Pos row col) color =
  cell (Pos (row * 2 + 1) (col * 4 + 2)) (char ' ' & bg color)

hwall :: Term.Cell
hwall = char '━'

vwall :: Term.Cell
vwall = char '┃'

------------------------------------------------------------------------------------------------------------------------
-- Rendering toolkit

newtype Scene
  = Scene (Pos -> Term.Scene)
  deriving newtype (Monoid, Semigroup)

renderSceneAt :: Pos -> Scene -> Term.Scene
renderSceneAt pos (Scene f) =
  f pos

cell :: Pos -> Term.Cell -> Scene
cell (Pos r c) x =
  Scene \(Pos r0 c0) ->
    Term.cell (Pos (r + r0) (c + c0)) x
