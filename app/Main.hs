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
    putStrLn "\n---------------------------------------------"
    putStrLn "\nTest defaultTokenizeAtom\n"
    putStrLn $ showMy $ tokenize defaultTokenizeAtom                               "qqq wwwwww  eeeeeeee rrrrrr"
    putStrLn ""
    putStrLn $ showMy $ tokenize defaultTokenizeAtom   {ta_start = Just ["w","r"]} "qqq wwwwww  eeeeeeee rrrrrr"
    putStrLn ""
    putStrLn $ showMy $ tokenize defaultTokenizeAtomDm                             "lala beb qq foo 12345 {lala bebe baar}"
    putStrLn $ showMy $ tokenize defaultTokenizeAtomDm {tad_start = Just ["d"]}    "lala beb qq foo 12345 {lala bebe baar}"
    --putStrLn $ showMy $ tokenize (TokenizeBlock [ ("{","}")
    --                                            , ("/*","*/")
    --                                            , ("\"","\"")
    --                                            ] Nothing True
    --                             ) 
    --                             "void func1 (var p, var t) { /* asasa */}"
    --                             "lala beb qq foo 12345 \"lala bebe \n qq \t baar\""
    putStrLn ""
    putStrLn "\nTest auxiliary functions\n"
    --putStrLn $ show $ breakOn "," "a::b::c"
    --putStrLn $ show $ recCrumbs ["{","}", " ", ","] ["void func1 (var p, var t) { /* asasa */}"]
    --putStrLn $ show $ recCrumbs ["{","}"] ["void func1 (var p, var t) { /* asasa */}"]
    --putStrLn $ show $ recCrumbsN "{" ["void func1 (var p, var t) { /* asasa */}"]
    --putStrLn $ show $ recCN "{" "void func1 (var p, var t) { /* asasa */}"



showMy :: Show a
       => [a]
       -> String
showMy [] =
    "[]"
showMy (x:[]) =
    "[" ++ show x ++ "]"
showMy v =
    "[ " ++ recS v ++ "\n]"
    where
        recS :: Show a => [a] -> String
        recS [] = ""
        recS (x:[]) = show x
        recS (x:xs) = show x ++ "\n, " ++ recS xs



