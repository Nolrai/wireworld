module Main (main) where

import WireWorld (WorldSize (..), neighborIndexes)

main :: IO ()
main = print $ neighborIndexes (WS [5, 5]) 11
