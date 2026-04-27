-- Definition of the Color and Tree type, along with what you need to add to them and check rb axioms, and the depth function
-- depth function is later useful for many recursive functions on trees i.e. most of what you want to do on a tree

module Rbt (
  Color(..), Tree(..), add, redAx, blackAx, rbAx, depth
) where

import Data.Maybe (isJust)

-- Red, Black and Nil (purple is the color in my head). What's the real difference between Nil nodes and Black nodes? 
-- The point is our list encoding format. It will add to the (criteria given in order) shallowest leftmost available adding location. 
-- We need the Nil node so that we know not to add to that part of the tree later.
-- In a different sense, the N node acts much like an empty tree: you can't add there. Thus most of this code will treat them in a similar way.
data Color = R | B | N deriving Show
data Tree = E | LeafN | T Color Tree Tree deriving Show 
-- It's kinda ugly that we have LeafN and smart constructors that produce that from a N added to empty. But oh well.

-- The addD function returns the depth of the location where addition occurred. 
-- This is to allow our recursive defintion to add to the least deep option.
addD :: Color -> Tree -> Maybe (Tree, Int) 
addD x LeafN = Nothing
addD N E = Just (LeafN, 0) -- add depths start at 0
addD x E = Just (T x E E, 0) -- ''
addD x (T c left right) = 
  case (leftAdd, rightAdd) of 
    (Nothing, Nothing) -> Nothing
    (Just (leftAddTree, leftDepth), Nothing) -> Just (T c leftAddTree right, leftDepth + 1) -- when we add the add depth goes up by 1 each time
    (Nothing, Just (rightAddTree, rightDepth)) -> Just (T c left rightAddTree, rightDepth + 1) -- ''
    -- Now for the main case, we want to add to the least deep location
    (Just (leftAddTree, leftDepth), Just (rightAddTree, rightDepth)) -> 
      if leftDepth <= rightDepth
        then Just (T c leftAddTree right, leftDepth + 1) -- the add depth goes up by 1 again
      else Just (T c left rightAddTree, rightDepth + 1) -- ''
  where leftAdd = addD x left
        rightAdd = addD x right
 
add :: Color -> Tree -> Maybe Tree
add x y = 
    case addD x y of 
      Nothing -> Nothing
      Just (tree, _) -> Just tree

-- redAx: no red node can be followed by a red node
redAx :: Tree -> Bool 
redAx E = True
redAx LeafN = True
redAx (T R (T R _ _) _) = False -- This and the next case cover for when red is followed by red.
redAx (T R _ (T R _ _)) = False -- ''
redAx (T c left right) = redAx left && redAx right -- If we don't have a red node at the root, then it satisfies redAx iff both branches do.

-- blackD is to blackAx as addD is to add. 
-- blackAx: For all nodes n, (for each path from n to a leaf, the path must past through the same number of blacks)
-- To check this we will have need of the blackD function, which returns that constant for the root of the tree 
-- (otherwise, it returns Nothing)
blackD :: Tree -> Maybe Int 
blackD E = Just 0
blackD LeafN = Just 0
blackD (T c left right) = 
  case (leftD, rightD) of 
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    (Just leftDepth, Just rightDepth) -> -- If each branch has the same depth, then blackAx is satisfied.
      if leftDepth == rightDepth
        then (
        case c of 
          B -> Just (leftDepth + 1) -- if the root is B then the blackD is commonDepth + 1
          _ -> Just (leftDepth) -- otherwise it is the common depth
        )
      else (Nothing) -- if the branches had different depths, then blackAx is violated, so we return Nothing
  where leftD = blackD left
        rightD = blackD right

blackAx :: Tree -> Bool
blackAx tree = isJust (blackD tree)

rbAx :: Tree -> Bool
rbAx tree = (redAx tree) && (blackAx tree)



depth :: Tree -> Int
depth E = -1
depth LeafN = 0
depth (T _ left right) = 1 + max (depth left) (depth right)

