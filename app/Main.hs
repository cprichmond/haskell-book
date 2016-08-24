t
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0


notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x     = Just x


replaceThe :: String -> String
replaceThe x
  | 1 <= (length . words) x = x
  | otherwise = replaceThe (concat . tail . words) x

main :: IO ()
main = print $ replaceThe "the cow loves us"
 
