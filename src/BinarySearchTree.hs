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
    = BST String BST BST
    -- | An empty Binary Search tree with no value or subtrees.
    | Null

-- | Shows the contents of the Binary Search Tree in order, using the showBST
-- function.
instance Show BST where
    show = showBST

{-|
  Returns a String representation of the contents of the given Binary Search
  Tree in order.

  >>> showBST BST "Ben" (BST "Alice" Null Null) (BST "Sam" Null Null)
  [Ben][Alice][Sam]
-}
showBST :: BST -> String
showBST (BST value left right) = showBST left ++ "[" ++ value ++ "]" ++ showBST right
showBST Null = ""

showPreBST :: BST -> String
showPreBST (BST value left right) = "[" ++ value ++ "]" ++ showPreBST left ++ showPreBST right
showPreBST Null = ""

showPostBST :: BST -> String
showPostBST (BST value left right) =  showPostBST left ++ showPostBST right ++ "[" ++ value ++ "]"
showPostBST Null = ""

{-|
  Inserts the given value into the given Binary Search Tree.

  >>> insertBST (BST "Ben" Null Null) "Alice"
  [Alice][Ben]
-}
insertBST :: BST -> String -> BST
insertBST (BST value left right) item
    | item <= value = BST value (insertBST left item) right
    | item > value = BST value left (insertBST right item)
insertBST Null item = BST item Null Null

{-|
  Inserts all of the values in the given list into the Binary Search tree.

  >>> insertAllBST (BST "Ben" Null Null) ["Alice", "Sam", "George"]
  [Alice][Ben][George][Sam]
-}
insertAllBST :: BST -> [String] -> BST
insertAllBST = foldl insertBST

{-|
  Returns a value indicating whether or not the Binary Search Tree contains the
  given value.

  >>> containsBST (BST "Ben" Null Null) "Ben"
  True
  >>> containsBST (BST "Ben" Null Null) "Alice"
  False
-}
containsBST :: BST -> String -> Bool
containsBST (BST value left right) item
    | item == value = True
    | item < value = containsBST left item
    | item > value = containsBST right item
containsBST Null item = False
