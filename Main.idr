module Main

import Data.Vect
import Control.ST
import Control.ST.ImplicitCall
import TicTacToe

start : ST m Var [add (State (Game Start InPlay))]
start = do game <- new initGame
           pure game

status : (game : Var) -> ST m GameState [game ::: State (Game b st)]
status game = pure $ currentStatus !(read game)

grid : (game : Var) -> ST m Grid [game ::: State (Game b st)]
grid game = pure $ currentGrid !(read game)

board : {b : Board g p turn} -> (game : Var) -> ST m (Board g p turn) [game ::: State (Game b st)]
board game = pure $ currentBoard !(read game)

move : {b : Board g p turn}
    -> (pos : Position)
    -> (game : Var)
    -> {auto prf : Empty pos g}
    -> ST m GameState [game ::: State (Game b InPlay) :->
                          (\st' => State (Game (Turn pos b {prf = prf}) st'))]
move pos game = do g <- read game
                   let (st' ** g') = move pos g
                   write game g'
                   pure st'

inputPosition : ConsoleIO m => ST m (Maybe Position) []
inputPosition = do putStr "Input x position: "
                   x <- inputNat
                   putStr "Input y position: "
                   y <- inputNat
                   pure [MkPair px py | x' <- x, y' <- y, px <- natToFin x' 3, py <- natToFin y' 3]
  where
    inputNat : ConsoleIO m => ST m (Maybe Nat) []
    inputNat = do input <- getStr
                  if all isDigit (unpack input)
                     then pure . Just . cast $ input
                     else pure Nothing

inputPosLoop : ConsoleIO m => ST m Position []
inputPosLoop = do Just pos <- inputPosition
                    | Nothing => do putStrLn "Bad input"
                                    inputPosLoop
                  pure pos

ExistBoard : Type
ExistBoard = (g : Grid ** p : Player ** turn : Nat ** Board g p turn)

WholeGame : (ExistBoard, EndState) -> Type
WholeGame ((_ ** _ ** _ ** b), res) = State (Game b (Ended res))

loop : ConsoleIO m
    => (game : Var)
    -> {b : Board g p turn}
    -> ST m (ExistBoard, EndState)
            [game ::: State (Game b InPlay) :-> WholeGame]
loop game {b} {g} = do putStrLn $ Grid.show g
                       pos <- inputPosLoop
                       case isEmpty pos g of
                            Yes prf => do InPlay <- move pos game
                                            | Ended end => pure ((_ ** _ ** _ ** _), end)
                                          loop game
                            No contra => do putStrLn "Position already occupied"
                                            loop game

main : IO ()
main = run $ do game <- start
                ((g ** _ ** _ ** b), res) <- loop game
                putStrLn $ Grid.show g
                case res of
                     Draw => do putStrLn "DRAW!"
                                delete game
                     Won p => do print p
                                 putStrLn " is the WINNER!"
                                 delete game
