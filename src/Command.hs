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
Module      : Command
Description : Defines CLI commands and includes functions for recognizing them.
Copyright   : (c) Christopher Wells, 2016
License     : MIT
Maintainer  : cwellsny@nycap.rr.com
-}
module Command where

-- | A representation of a program command.
data Command
    -- | A command to insert a String into the tree
    = Insert String
    -- | A command to check if the given String is in the tree
    | Contains String
    -- | A command to display the contents of the tree in order
    | InOrder
    -- | A command to display the contents of the tree in pre-order
    | PreOrder
    -- | A command to display the contents of the tree in post-order
    | PostOrder
    -- | A command to quit the program
    | Quit

-- | Shows the command and its contents using the showCommand function.
instance Show Command where
    show = showCommand

{-|
  Returns a String representation of the given Command.

  >>> showCommand (Insert "Alice")
  "Insert Alice"
-}
showCommand :: Command -> String
showCommand command = case command of
    Insert item -> "Insert " ++ item
    Contains item -> "Contains " ++ item
    InOrder -> "InOrder"
    PreOrder -> "PreOrder"
    PostOrder -> "PostOrder"
    Quit -> "Quit"

{-|
  Attempts to convert the given String into a CLI command.

  >>> toCommand "i Alice"
  Just Insert Alice
  >>> toCommand "pre"
  Just PreOrder
  >>> toCommand "test"
  Nothing
-}
toCommand :: String -> Maybe Command
toCommand commandString = case words commandString of
    ["i", item] -> Just $ Insert item
    ["c", item] -> Just $ Contains item
    ["in"] -> Just InOrder
    ["pre"] -> Just PreOrder
    ["post"] -> Just PostOrder
    ["q"] -> Just Quit
    _ -> Nothing
