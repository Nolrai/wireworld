module Main (main) where

import WireWorld (neighborIndexes,  WorldSize(..))


main :: IO ()
main = print $ neighborIndexes (WS [5,5]) 11
