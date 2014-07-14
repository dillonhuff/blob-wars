module BoardTests

import BlobBoard
import TestUtils

getCases : Vect 5 (Pair BoardLocation Blob)
getCases =
	 [MkPair (bl 0 0) Empty
	 ,MkPair (bl 9 9) Empty
	 ,MkPair (bl 0 6) Blue
	 ,MkPair (bl 0 1) Red
	 ,MkPair (bl 2 0) Empty]

testBoard : Board
testBoard = place Blue (bl 0 6) $ place Red (bl 0 1) $ emptyBoard

testGet : String
testGet = testFunction (get testBoard) getCases

adjacentCases : Vect 1 (Pair BoardLocation (List BoardLocation))
adjacentCases =
	      [MkPair (bl 0 0)
	      	      [bl 1 0, bl 0 1, bl 1 1]]

testAdjacent : String
testAdjacent = testFunction adjacent adjacentCases