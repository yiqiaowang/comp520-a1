module Main(main) where

import Scanner (scanToken)
import Parser (parseInput)


isValid :: Bool -> String
isValid True = "VALID"
isValid _ = "INVALID"

  
main = do
  input <- getContents
  putStrLn ((isValid . parseInput . scanToken) input)
