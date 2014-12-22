module KeyEvent where

data KeyboardEvent = No | MoveLeftward | MoveRightward | MoveForward | MoveBackward | Fire | Pause
          deriving (Eq, Show)

{- Кодам соответствуют русские символы -}
event :: Char -> KeyboardEvent
event ch
  | ch=='a' || ch=='A' || ch=='\244' || ch=='\212' = MoveLeftward
  | ch=='d' || ch=='D' || ch=='\226' || ch=='\194' = MoveRightward
  | ch=='w' || ch=='W' || ch=='\246' || ch=='\214' = MoveForward
  | ch=='s' || ch=='S' || ch=='\251' || ch=='\219' = MoveBackward
  | ch=='p' || ch=='P' || ch=='\231' || ch=='\199' = Pause
  | ch==' ' = Fire
  | otherwise = No
