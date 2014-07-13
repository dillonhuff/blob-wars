module BoardTests

import BlobBoard
import TestUtils

getCases : Vect 5 (Pair BoardLocation Blob)
getCases =
	 [MkPair (BoardLoc 0 0) Empty
	 ,MkPair (BoardLoc 9 9) Empty
	 ,MkPair (BoardLoc 0 6) Blue
	 ,MkPair (BoardLoc 0 1) Red
	 ,MkPair (BoardLoc 2 0) Empty]

testBoard : Board
testBoard = place Blue (bl 0 6) $ place Red (bl 0 1) $ emptyBoard

testGet : String
testGet = testFunction (get testBoard) getCases