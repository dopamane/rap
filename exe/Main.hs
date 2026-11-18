module Main (main) where

import Data.Rap

main :: IO ()
main = putStrLn $ "Hello, Haskell!" ++ " " ++ show (Verse (Four 0 0 0 0) mempty)
