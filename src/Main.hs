module Main where

import Test.DocTest

import TrieBasedFRP


-- TODO: demonstrate how to build and use a dynamic event network.

main :: IO ()
main = doctest ["-isrc", "src/Main.hs"]
