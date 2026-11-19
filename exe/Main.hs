module Main (main) where

import Data.Rap

main :: IO ()
main = putStrLn $ "Hello, Haskell!" ++ " " ++ show ([] :: Rap)
