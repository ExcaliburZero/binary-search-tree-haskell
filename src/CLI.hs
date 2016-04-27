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
Module      : CLI
Description : Contains functions for the command line interface.
Copyright   : (c) Christopher Wells, 2016
License     : MIT
Maintainer  : cwellsny@nycap.rr.com
-}
module CLI where

import BinarySearchTree

import Control.Monad (when, unless)

{-|
  Prompts the user for a command and returns the given command.

  >>> promptForCommand
  Enter a command (i, c, in, pre, post, or q):
  i
-}
promptForCommand :: IO String
promptForCommand = do
    putStrLn "Enter a command (i, c, in, pre, post, or q):"
    getLine

{-|
  Validates the given command.

  >>> validateCommand "i Alice"
  True
  >>> validateCommand "show Alice"
  False
  >>> validateCommand ""
  False
-}
validateCommand :: String -> Bool
validateCommand x
    | x == "" = False
    | commandType == "i" && commandLength == 2 = True
    | commandType == "c" && commandLength == 2 = True
    | commandType == "in" && commandLength == 1 = True
    | commandType == "pre" && commandLength == 1 = True
    | commandType == "post" && commandLength == 1 = True
    | commandType == "q" && commandLength == 1 = True
    | otherwise = False
    where commandType = head (words x)
          commandLength = length (words x)

{-|
  Prints the contents of the given tree with the given show function.
-}
printTree :: BST -> (BST -> String) -> IO BST
printTree b f = do
    let result = f b
    let resultStr = if null result then "The tree has no nodes." else result
    putStrLn resultStr
    return b

{-|
  Prints a String representing whether or not the given object is contained
  within the given Binary Search Tree.
-}
printContains :: BST -> String -> IO BST
printContains b item = do
    let result = if containsBST b item then item ++ " is contained in the tree." else item ++ " is not contained in the tree."
    putStrLn result
    return b

{-|
  Preforms an action based on the user entered command.
-}
action :: BST -> IO ()
action b = do
    command <- promptForCommand
    let valid = validateCommand command
    unless valid $ printInvalid command
    bs <- runCommand b command
    when (command /= "q") $ action bs

{-|
  Runs the given command on the given Binary Search Tree, and returns the resulting Binary Search Tree.
-}
runCommand :: BST -> String -> IO BST
runCommand b c
    | c == "" = return b
    | commandType == "i" = return (insertBST b item)
    | commandType == "c" = printContains b item
    | commandType == "in" = printTree b showBST
    | commandType == "pre" = printTree b showPreBST
    | commandType == "post" = printTree b showPostBST
    | commandType == "q" = return b
    | otherwise = return b
    where commandType = head (words c)
          item = last $ words c

{-|
  Prints that an invalid command was entered.

  >>> printInvalid "test Alice"
  Invalid command -- test Alice
-}
printInvalid :: String -> IO ()
printInvalid s = putStrLn ("Invalid command -- " ++ s)
