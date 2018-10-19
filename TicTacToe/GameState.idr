module TicTacToe.GameState

import TicTacToe.Player

%default total
%access export

public export
data EndState = Draw
              | Won Player

public export
data GameState = InPlay
               | Ended EndState

Show EndState where
  show Draw = "Draw"
  show (Won p) = "Won " ++ show p

Show GameState where
  show InPlay = "InPlay"
  show (Ended s) = "Ended " ++ show s
