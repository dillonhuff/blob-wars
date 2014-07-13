module BlobBoard

data Blob = Red | Blue | Empty

instance Show Blob where
  show Red = "@"
  show Blue = "!"
  show Empty = " "

instance Eq Blob where
  (==) Red Red = True
  (==) Blue Blue = True
  (==) Empty Empty = True
  (==) _ _ = False

Board : Type
Board = Vect 10 (Vect 10 Blob)

data BoardLocation = BoardLoc (Fin 10) (Fin 10)

bl : Fin 10 -> Fin 10 -> BoardLocation
bl x y = BoardLoc x y

getCol : BoardLocation -> Fin 10
getCol (bl x y) = x

getRow : BoardLocation -> Fin 10
getRow (bl x y) = y

get : Board -> BoardLocation -> Blob
get board loc = index (getRow loc) (index (getCol loc) board)

place : Blob -> BoardLocation -> Board -> Board
place b loc board = let column = index (getCol loc) board in
 let newColumn = replaceAt (getRow loc) b column in
 replaceAt (getCol loc) newColumn board

emptyBoard : Board
emptyBoard = replicate 10 (replicate 10 Empty)