# 776_Practical 
Sudoku Solver in Haskell

First attempt used pencil-and-paper algorithms to solve puzzles.
Using two simple algorithms, it was able to solve easy puzzles, but nothing more difficult.

My second attempt used a DFS to search for a solution to a puzzle.
It is able to, presumably, find a solution to any puzzle.  Solves "simple" easy, medium, hard puzzles in under five seconds.
Solves the hardest puzzle I could find in approximately four minutes.  The increase in time is likely due to the low number of clue leading to several times more possible paths to search down.
