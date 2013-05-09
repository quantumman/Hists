{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Import
import Test.Hspec

import HistsTests


main :: IO ()
main = hspec $ foldl1' specs
