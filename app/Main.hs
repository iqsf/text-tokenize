{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------
-- Main module of application
----------------------------------------------------------------

module Main where

-- Import of modules
import           Data.Text               as T

import           TextTokenize.Parser



-- | Entry point in the program
main :: IO ()
main = do 
    putStrLn "\nTest defaultTokenizeProps\n"
    putStrLn $ show $ tokenize defaultTokenizeProps                             "qqq wwwwww  eeeeeeee rrrrrr"
    putStrLn ""
    putStrLn $ show $ tokenize defaultTokenizeProps {tp_start = Just ["w","r"]} "qqq wwwwww  eeeeeeee rrrrrr"
    putStrLn ""
    putStrLn $ show $ tokenize (TokenizeBlock [ ("{","}")
                                              , ("/*","*/")
                                              , ("\"","\"")
                                              ] Nothing True
                               ) 
    --                           "void func1 (var p, var t) { /* asasa */}"
                               "lala beb qq foo 12345 \"lala bebe \n qq \t baar\""
    putStrLn ""
    putStrLn "\nTest auxiliary functions\n"
    --putStrLn $ show $ breakOn "," "a::b::c"
    --putStrLn $ show $ recCrumbs ["{","}", " ", ","] ["void func1 (var p, var t) { /* asasa */}"]
    --putStrLn $ show $ recCrumbs ["{","}"] ["void func1 (var p, var t) { /* asasa */}"]
    --putStrLn $ show $ recCrumbsN "{" ["void func1 (var p, var t) { /* asasa */}"]
    --putStrLn $ show $ recCN "{" "void func1 (var p, var t) { /* asasa */}"




