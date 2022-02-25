module Lib where

import Var

someFunc :: IO ()
someFunc = putStrLn "someFunc"

myDec =
  [d|
    myFunction :: Int -> Int
    myFunction x =
      if (==) x 0
      then 0
      else myFunction 0
  |]
