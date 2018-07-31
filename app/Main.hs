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
    --putStrLn ""
    putStrLn $ showMy $ tokenize defaultTokenizeAtom   {ta_start = Just ["w","r"]} "qqq wwwwww  eeeeeeee rrrrrr"
    --putStrLn ""
    --putStrLn $ showMy $ tokenize defaultTokenizeAtomDm                             "lala beb qq foo 12345 {lala bebe baar}"
    --putStrLn $ showMy $ tokenize defaultTokenizeAtomDm {tad_start = Just ["d"]}    "lala beb qq foo 12345 {lala bebe baar}"
--    putStrLn $ show "Aaaaa \"bbb\""
    --putStrLn $ show "Aaaaa \"bbb\" \" \"cc\""
    --putStrLn $ show "Aaaaa \"bbb\" \"\ncc\""
    --putStrLn $ show "Aaaaa \"bbb\" \"cc\""
--    putStrLn $ showMy $ tokenize defaultTokenizeAtomDm {tad_splits = ["\"", "\n"]} "Aaaaa \"bbb\""
    --putStrLn $ showMy $ tokenize defaultTokenizeAtomDm {tad_splits = ["\"", "\n"]} "Aaaaa \"bbb\" \" \"cc\""
    --putStrLn $ showMy $ tokenize defaultTokenizeAtomDm {tad_splits = ["\"", "\n"]} "Aaaaa \"bbb\" \"\ncc\""
    --putStrLn $ showMy $ tokenize defaultTokenizeAtomDm {tad_splits = ["\"", "\n"]} "Aaaaa \"bbb\" \"cc\""
    
    putStrLn ""
    putStrLn "\nTest defaultTokenizeForString\n"
    --putStrLn $ showMy $ tokenize defaultTokenizeForString                          "lala beb qq foo 12345 \"lala bebe baar\""
    putStrLn $ show                                                                "Aaaaa \"bbb\""
    putStrLn $ showEt $ tokenize defaultTokenizeForString                          "Aaaaa \"bbb\""
    putStrLn $ show                                                                "Aaaaa \"bb\"b\""
    putStrLn $ showEt $ tokenize defaultTokenizeForString                          "Aaaaa \"bb\"b\""
    --putStrLn $ showEt $ tokenize defaultTokenizeForString                          "Aaaaa \"bbb\" \" \"cc\""
    --putStrLn $ showEt $ tokenize defaultTokenizeForString                          "Aaaaa \"bbb\" \"\ncc\""
    --putStrLn $ showEt $ tokenize defaultTokenizeForString                          "Aaaaa \"bbb\" \"cc\""

    putStrLn ""
    putStrLn "\nTest auxiliary functions\n"
    --putStrLn $ show $ recAtom [" "] ["qq  wwwww  eeeeee rrrrr"]

    --putStrLn $ show $ breakOn "," "a::b::c"
    --putStrLn $ show $ recCrumbs ["{","}", " ", ","] ["void func1 (var p, var t) { /* asasa */}"]
    --putStrLn $ show $ recCrumbs ["{","}"] ["void func1 (var p, var t) { /* asasa */}"]
    --putStrLn $ show $ recCrumbsN "{" ["void func1 (var p, var t) { /* asasa */}"]
    --putStrLn $ show $ recCN "{" "void func1 (var p, var t) { /* asasa */}"


showEt :: Show a
       => Either Text [a]
       -> String
showEt (Left err) = 
    show err
showEt (Right v) = 
    showMy v

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



