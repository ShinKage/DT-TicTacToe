module TicTacToe.Player

%default total
%access export

public export
data Player = Cross
            | Nought

public export
Eq Player where
  Cross  == Cross  = True
  Nought == Nought = True
  _ == _ = False

Show Player where
  show Cross  = "X"
  show Nought = "O"

showCell : Maybe Player -> String
showCell Nothing  = " "
showCell (Just c) = show c

public export
switch : Player -> Player
switch Cross = Nought
switch Nought = Cross
