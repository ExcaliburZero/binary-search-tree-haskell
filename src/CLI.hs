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
    | head (words x) == "i"  && (length (words x) == 2) = True
    | head (words x) == "c"  && (length (words x) == 2) = True
    | head (words x) == "in"  && (length (words x) == 1) = True
    | head (words x) == "pre"  && (length (words x) == 1) = True
    | head (words x) == "post"  && (length (words x) == 1) = True
    | head (words x) == "q"  && (length (words x) == 1) = True
    | otherwise = False

{-|
  Preforms an action based on the user entered command.
-}
action :: BST -> IO ()
action b = do
    command <- promptForCommand
    let valid = validateCommand command
    unless valid $ printInvalid command
    when (command /= "q") $ action b

{-|
  Prints that an invalid command was entered.

  >>> printInvalid "test Alice"
  Invalid command -- test Alice
-}
printInvalid :: String -> IO ()
printInvalid s = putStrLn ("Invalid command -- " ++ s)
