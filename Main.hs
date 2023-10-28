module Main
  ( main,
  )
where

import Data.Foldable (fold)
import Data.Function ((&))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Exit (exitFailure)
import Termbox.Banana (Key (..), Pos (..), char)
import Termbox.Banana qualified as Term

main :: IO ()
main =
  Term.run main1 >>= \case
    Left _err -> exitFailure
    Right () -> pure ()

main1 :: Term.Inputs -> MomentIO (Term.Outputs ())
main1 Term.Inputs {keys} = do
  let bgame =
        pure
          Game
            { horizontalWalls = initialHorizontalWalls,
              verticalWalls = initialVerticalWalls
            }
  pure
    Term.Outputs
      { scene = renderSceneAt (Pos 0 0) . render <$> bgame,
        done = () <$ filterE (== KeyEsc) keys
      }

data Game = Game
  { horizontalWalls :: !(Vector IntSet),
    verticalWalls :: !(Vector IntSet)
  }

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

render :: Game -> Scene
render Game {horizontalWalls, verticalWalls} =
  fold
    [ renderHorizontalWalls horizontalWalls,
      renderVerticalWalls verticalWalls,
      renderIntersections horizontalWalls verticalWalls
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

--   0   1   2   3   4
-- 0 +---+---+---+---+
--   |   |   |   |   |
-- 1 +---+---+---+---+
--   |   |   |   |   |
-- 2 +---+---+---+---+
--   |   |   |   |   |
-- 3 +---+---+---+---+
--   |   |   |   |   |
-- 4 +---+---+---+---+

--   for row := 0 to row := length rows
--     for col := 0 to length cols
--       what character goes at cell (row*2, col*4)?
--       if there's a wall at

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
