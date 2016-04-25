{-
 - Copyright (c) 2016 Christopher Wells <cwellsny@nycap.rr.com>
 - 
 - Permission is hereby granted, free of charge, to any person obtaining a copy
 - of this software and associated documentation files (the "Software"), to deal
 - in the Software without restriction, including without limitation the rights
 - to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 - copies of the Software, and to permit persons to whom the Software is
 - furnished to do so, subject to the following conditions:
 -
 - The above copyright notice and this permission notice shall be included in
 - all copies or substantial portions of the Software.
 -
 - THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 - IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 - FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 - AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 - LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 - OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 - SOFTWARE.
 -}
{-|
Module      : BinarySearchTree
Description : Defines the BST data structure and includes functions for
              operating on instances of it.
Copyright   : (c) Christopher Wells, 2016
License     : MIT
Maintainer  : cwellsny@nycap.rr.com
-}
module BinarySearchTree where

-- | A representation of a Binary Search Tree.
data BST
    -- | A Binary Search Tree with a value and two subtrees.
    = BST Int BST BST
    -- | An empty Binary Search tree with no value or subtrees.
    | Null

-- | Shows the contents of the Binary Search Tree in order, using the showBST
-- function.
instance Show BST where
    show = showBST

{-|
  Returns a String representation of the contents of the given Binary Search
  Tree in order.

  >>> showBST BST 180 (BST 150 Null Null) (BST 190 Null Null)
  [150][180][190]
-}
showBST :: BST -> String
showBST (BST int left right) = showBST left ++ "[" ++ show int ++ "]" ++ showBST right
showBST Null = ""

{-|
  Inserts the given value into the given Binary Search Tree.

  >>> insertBST (BST 180 Null Null) 150
  [150][180]
-}
insertBST :: BST -> Int -> BST
insertBST (BST value left right) item
    | item <= value = BST value (insertBST left item) right
    | item > value = BST value left (insertBST right item)
insertBST Null item = BST item Null Null

{-|
  Inserts all of the values in the given list into the Binary Search tree.

  >>> insertAllBST (BST 180 Null Null) [150, 175, 190]
  [150][175][180][190]
-}
insertAllBST :: BST -> [Int] -> BST
insertAllBST = foldl insertBST

{-|
  Returns a value indicating whether or not the Binary Search Tree contains the
  given value.

  >>> containsBST (BST 180 Null Null) 180
  True
  >>> containsBST (BST 180 Null Null) 200
  False
-}
containsBST :: BST -> Int -> Bool
containsBST (BST value left right) item
    | item == value = True
    | item < value = containsBST left item
    | item > value = containsBST right item
containsBST Null item = False
