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
module BinarySearchTree where

data BST = BST Int BST BST | Null

instance Show BST where
    show bst = showBST bst

{-|
 - Prints the given Binary Search Tree.
 -}
showBST :: BST -> String
showBST (BST int left right) = showBST left ++ "[" ++ show int ++ "]" ++ showBST right
showBST Null = ""

{-|
 - Inserts the given value into the given Binary Search Tree.
 -}
insertBST :: BST -> Int -> BST
insertBST (BST value left right) item
    | item <= value = BST value (insertBST left item) right
    | item > value = BST value left (insertBST right item)
insertBST (Null) item = BST item Null Null

{-|
 - Returns a value indicating whether or not the Binary Search Tree contains the
 - given value.
 -}
containsBST :: BST -> Int -> Bool
containsBST (BST value left right) item
    | item == value = True
    | item < value = containsBST left item
    | item > value = containsBST right item
containsBST (Null) item = False
