module BlobBoard

showFin10 : Integer -> Fin 10 -> String
showFin10 n fZ = show n
showFin10 n (fS k) = showFin10 (n+1) k
--showFin10 
--  (fS k) => "n"
--  (fS k) => showFin10 (S n) k

instance Show (Fin 10) where
  show f = "n"

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

instance Show BoardLocation where
  show (BoardLoc r c) = show r ++ " " ++ show c

instance Eq BoardLocation where
  (==) (BoardLoc r1 c1) (BoardLoc r2 c2) = r1 == r2 && c1 == c2

bl : Fin 10 -> Fin 10 -> BoardLocation
bl x y = BoardLoc x y

getCol : BoardLocation -> Fin 10
getCol (bl x y) = x

getRow : BoardLocation -> Fin 10
getRow (bl x y) = y

get : Board -> BoardLocation -> Blob
get board loc = index (getCol loc) (index (getRow loc) board)

place : Blob -> BoardLocation -> Board -> Board
place b loc board = let row = index (getRow loc) board in
 let newRow = replaceAt (getCol loc) b row in
 replaceAt (getRow loc) newRow board

emptyBoard : Board
emptyBoard = replicate 10 (replicate 10 Empty)

adjacent : BoardLocation -> List BoardLocation
adjacent b = []