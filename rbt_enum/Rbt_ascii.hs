-- Makes color coded (using terminal colors) ascii art of the trees.
module Rbt_ascii (
  ascii, asciiC, colorReplace
) where 

import Data.List.Split
import Data.List (isPrefixOf)
import Rbt


replace :: String -> String -> String -> String
replace needle replacement = go
  where -- go is a helper function
  go [] = []
  go s@(x:xs) -- the variable s stores (x:xs), giving us a convenient handle for the whole list, while xs is the part without the head x.
    | needle `isPrefixOf` s = replacement ++ go (drop (length needle) s) -- replacement + (rest of string without the needle)
    | otherwise = x : go xs -- if we don't have it as a prefix then we shift our position by one and then check again


-- The general strategy is to assemble the art in "blocks". A tree is formed by connecting the root to the block of each subtree.


-- asciiTotArm takes in the depth of a tree and spits out how many arms (the backslash and forwardslash characters connecting nodes) we need for the leftmost and rightmost two arms.
asciiTotArm :: Int -> Int
asciiTotArm 0 = 0
-- asciiTotArm depth = (2 * (depth - 1) + 1) * (depth - 1) + 1
asciiTotArm depth = (2 * (depth - 1) + 1) * (depth - 1) + 1 -- num arms on each connection is a linear prog, so  the partial sum is quadratic.
-- leftArm is preceded by totArm + depth + 1 spaces
-- except that we need to plug in the depth one before
-- This should be num arms + num nodes in the way. num nodes = depth + 1
asciiStart :: Int -> Int
asciiStart depth = asciiTotArm depth + depth + 1
-- How wide should the whole block be? This is just (start to middle) * 2 + one for middle. start to middle is asciiStart depth - 1 since asciiStart includes the middle
asciiWidth :: Int -> Int
asciiWidth depth = 1 + 2 * (asciiStart depth - 1)


-- if we have a smaller tree that we add to a root, we need to pad it. the width gets bigger, and there are more empty rows at the bottom
-- To do this we "rowify" the block, turning it into a list of strings, one for each row.
pad :: Int -> Int -> Int -> Int -> [String] -> [String]
pad toWidth toHeight alreadyWidth alreadyHeight alreadyRows = 
  widthPaddedRows ++ bottomSpaces -- first we add spaces to each row, to pad them to the right width. then we add the extra rows of spaces at the bottom
  where difWidth = toWidth - alreadyWidth
        sideNumSpaces = div difWidth 2
        fullSpaces = replicate toWidth ' '
        sideSpaces = replicate sideNumSpaces ' '
        difHeight = toHeight - alreadyHeight
        widthPaddedRows = [sideSpaces ++ row ++ sideSpaces | row <- alreadyRows] 
        bottomSpaces = replicate difHeight fullSpaces

-- given two rowified blocks, pad the smaller one and return both (in the order it was given)
padRowPair :: [String] -> [String] -> ([String], [String])
padRowPair leftRows rightRows
  | leftWidth == rightWidth = (leftRows, rightRows)
  | leftWidth < rightWidth = (pad rightWidth rightHeight leftWidth leftHeight leftRows, rightRows)
  | leftWidth > rightWidth = (leftRows, pad leftWidth leftHeight rightWidth rightHeight rightRows)
  where leftHeight = length leftRows
        rightHeight = length rightRows
        leftWidth = length (head leftRows)
        rightWidth = length (head rightRows)

-- given the block strings of two subtrees, put them together
rowsCat :: String -> String -> String
rowsCat left right = init (foldl (++) "" --init is to remove the last \n
                          (zipWith (\l -> \r -> l ++ (replicate 3 ' ') ++ r ++ "\n")  -- put three spaces between the two parts. this is spacing rightest leafspot of left with leftest leafspot of right
                          (fst paddedRowPair) (snd paddedRowPair))) 
  where leftRows = splitOn "\n" left
        rightRows = splitOn "\n" right
        paddedRowPair = padRowPair leftRows rightRows

-- creates the ith row of the new arms we are adding at the top
-- each row is some spaces followed by / followed by some spaces followed by \ followed by spaces followed by \n
-- as we go down the rows, the / moves one to the right, and the \ moves one to the left
-- thus the middle whitespace gets smaller by 2, and the side whisespaces each get bigger by 1
topArmsRowI :: Int -> Int -> String
topArmsRowI depth i  = 
  (replicate (lStart + i) ' ') ++ 
  "/" ++ 
  (replicate (middle - 2 * i) ' ') ++ 
  "\\" ++ 
  (replicate (lStart + i) ' ') ++ "\n"
  where width = asciiWidth depth
        lStart = asciiStart (depth - 1)
        middle = width - 2 * lStart - 2

-- the height from the top to the next level is 2^depth - 2, so now create those top arm rows
topArms :: Int -> String
topArms depth = foldl (++) "" (map (topArmsRowI depth) (reverse [0..(2^depth - 2)]))


ascii :: Tree -> String
ascii E = "E"
ascii LeafN = "N"
ascii (T c E E) = 
  case c of
    R -> "R"
    B -> "B"
ascii (T c left right) = topRow ++ topArms thisDepth ++ 
                        (rowsCat (ascii left) (ascii right))
  where leftAscii = ascii left
        leftD = depth left
        rightAscii = ascii right
        rightD = depth right
        thisDepth = 1 + max leftD rightD
        topLeft = (asciiStart thisDepth) - 1 
        topRow = (replicate topLeft ' ') ++ (ascii (T c E E)) ++ (replicate topLeft ' ') ++ "\n"

colorReplace :: String -> String
colorReplace str = foldr ($) (str) (map (uncurry replace) (zip froms tos)) -- successively apply the necessary replacements. the map returns a list of replacing functions
  where froms = ["R", "B", "N"]
        tos = ["\ESC[31mR\ESC[0m", "\ESC[90mB\ESC[0m", "\ESC[38;5;90mN\ESC[0m"] -- terminal color codes TODO: make/use a general colorize function

asciiC :: Tree -> String
asciiC t = colorReplace (ascii t)
