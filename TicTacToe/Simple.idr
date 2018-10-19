module TicTacToe.Simple

import TicTacToe.GameState
import TicTacToe.Player

import Data.Matrix

%access export
%default total

public export
Grid : Type
Grid = Matrix 3 3 (Maybe Player)

public export
Position : Type
Position = (Fin 3, Fin 3)

namespace Grid
  export
  show : Grid -> String
  show g = unlines . intersperse rule . toList . map row $ g
    where
      row : Vect 3 (Maybe Player) -> String
      row = strCons ' ' . concat . intersperse " | " . map showCell

      rule : String
      rule = pack $ List.replicate 11 '-'
      --rule = pack $ List.replicate (n * 3 + (n `minus` 1)) '-'

public export
emptyGrid : Grid
emptyGrid = replicate _ . replicate _ $ Nothing

public export
get : Position -> Grid -> Maybe Player
get (x, y) = indices x y

public export
set : Player -> Position -> Grid -> Grid
set p (x, y) g = replaceAt x (replaceAt y (Just p) (getRow x g)) g

public export
occupied : Grid -> Nat
occupied = sum . map (DPair.fst . filter isJust)

public export
free : Position -> Grid -> Bool
free (x, y) = isNothing . indices x y

public export
hasWinner : Grid -> Maybe Player
hasWinner g = List.find pred (rows g) >>= Vect.head
  where
    rows : Grid -> List (Vect 3 (Maybe Player))
    rows m = (diag m) :: (diag . reverse $ m) :: toList m ++ (toList $ transpose m)

    pred : Vect 3 (Maybe Player) -> Bool
    pred v = all (== Just Cross) v || all (== Just Nought) v

public export
data IsNothing : Maybe a -> Type where
  ItIsNothing : IsNothing Nothing

public export
Uninhabited (IsNothing (Just v)) where
  uninhabited ItIsNothing impossible

public export
isItNothing : (v : Maybe a) -> Dec (IsNothing v)
isItNothing Nothing = Yes ItIsNothing
isItNothing (Just _) = No absurd

public export
data Empty : (pos : Position) -> (g : Grid) -> Type where
  IsEmpty : {auto prf : IsNothing (get pos g)} -> Empty pos g

public export
isEmptyJust : (contra : IsNothing (get pos g) -> Void) -> Empty pos g -> Void
isEmptyJust contra (IsEmpty {prf}) = contra prf

isEmpty : (pos : Position) -> (g : Grid) -> Dec (Empty pos g)
isEmpty pos g with (isItNothing (get pos g))
  isEmpty _ _ | (Yes prf) = Yes IsEmpty
  isEmpty _ _ | (No contra) = No (isEmptyJust contra)

public export
data Board : (g : Grid) -> (p : Player) -> (turn : Nat) -> Type where
  Start : Board Simple.emptyGrid Cross 0
  Turn  : (pos : Position)
       -> Board g p turn
       -> {auto prf : Empty pos g}
       -> Board (set p pos g) (switch p) (S turn)

Show (Board g p turn) where
  show {g} _ = Grid.show g

public export
status : Board g p turn -> GameState
status {g} {turn} _ = case hasWinner g of
                           Nothing => if turn == 9
                                         then Ended Draw
                                         else InPlay
                           Just winner => Ended (Won winner)

public export
data Game : {g : Grid} -> (b : Board g p turn) -> GameState -> Type where
  MkGame : (b : Board g p turn) -> Game b (status b)

Show (Game b state) where
  show (MkGame {g} _) = Grid.show g

initGame : Game Start InPlay
initGame = MkGame Start

move : {b : Board g p turn}
    -> (pos : Position)
    -> Game b InPlay
    -> {auto prf : Empty pos g}
    -> (st : GameState ** Game (Turn pos b {prf = prf}) st)
move pos _ {b} {prf} = (_ ** MkGame (Turn pos b {prf = prf}))

winner : Game b (Ended (Won p)) -> Player
winner {p} _ = p

currentStatus : Game b st -> GameState
currentStatus {st} _ = st

currentGrid : Game b st -> Grid
currentGrid (MkGame {g} _) = g

currentBoard : {b : Board g p turn} -> Game b st -> Board g p turn
currentBoard (MkGame b) = b
