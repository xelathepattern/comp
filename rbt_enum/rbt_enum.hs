-- enumerate all the rbtrees whose encodings are of a specified length
import Data.Maybe (isJust, fromJust)
import Data.Bifunctor (bimap, first, second)
import Rbt
import Rbt_ascii

-- apply a function to the nth element of a list
nth :: Int -> (a -> a) -> [a] -> [a]
nth _ _ [] = []
nth 0 f (x:xs) = (f x) : xs
nth n f (x:xs) = x : (nth (n-1) f xs)

-- take a list of colors and a list of tree encoding pairs and spit out the list of tree encoding pairs corresponding to adding a color from colors to the trees
-- that is, grow the candidates by each color from [Color]
growCandidates :: [Color] -> [(Tree, [Color])] -> [(Tree, [Color])]
growCandidates colors candidates = foldl (++) [] (map (\c ->
                          (map (first fromJust) (filter (isJust . fst)
                          (map (bimap (add c) (c:)) candidates)
                          ))
                    ) colors)

-- the redAx satisfying trees can only come from growing a redAx satisfying tree. contains trailing Ns in encoding.
rtsN :: Int -> [(Tree, [Color])]
rtsN 0 = [(E, [])]
rtsN n = filter (redAx . fst) (growCandidates [R,B,N] (rtsN (n-1)))

-- the rbAx satisfying trees might come from ones that weren't themselves blackAx satisfying. thus we have to grow the redAx satisfying trees
rbtsN :: Int -> [(Tree, [Color])]
rbtsN 0 = [(E, [])]
rbtsN n = filter (rbAx . fst) (growCandidates [R,B,N] (rtsN (n-1)))

-- just like rbtsN but without trailing Ns.
-- we thus only add non nils to redAx satisfying trees and then filter for rbAx satisfaction
rbts :: Int -> [(Tree, [Color])] 
rbts n = filter (rbAx . fst) (growCandidates [R,B] (rtsN (n-1)))

colorCounts :: [Color] -> [Int]
colorCounts [] = [0,0,0]
colorCounts (R:xs) = nth 0 (1+) (colorCounts xs)
colorCounts (B:xs) = nth 1 (1+) (colorCounts xs)
colorCounts (N:xs) = nth 2 (1+) (colorCounts xs)

-- aesthetic requirements: 
-- each color is present more than twice
-- there aren't more than 3 blacks on any hand
aesthetics :: [Color] -> Bool
aesthetics xs = (all (>2) counts) && 
                ((lCounts !! 1) < 3) && 
                ((rCounts !! 1) < 3)
  where counts = colorCounts xs
        lCounts = colorCounts (take 5 xs)
        rCounts = colorCounts (take 5 (reverse xs))

printTrees :: [(Tree, [Color])] -> IO ()
printTrees = mapM_ $ \(tree, colors) -> do
                        (putStrLn . asciiC) tree
                        (putStrLn . colorReplace . show . reverse) colors

n = 10
main = printTrees (filter (aesthetics . snd) (rbts n))
--main = printTrees (rbts n)
