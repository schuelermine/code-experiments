{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Data.Char (type Char)
import Data.List ((++))
import System.IO (getLine, putStr, putStrLn, type IO)

main :: IO ()
main =
  ( do
      (putStr :: [Char] -> IO ()) ("What is your name?  " :: [Char]) :: IO ()
      name :: [Char] <- getLine :: IO [Char]
      (putStrLn :: [Char] -> IO ()) ((((++) :: [Char] -> ([Char] -> [Char])) ("Hello, " :: [Char]) :: [Char] -> [Char]) (name :: [Char]) :: [Char]) :: IO ()
  ) ::
    IO ()
