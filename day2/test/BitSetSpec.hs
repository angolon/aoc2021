{-# LANGUAGE TemplateHaskell #-}

module BitSetSpec where

import BitSet
import Data.Foldable
import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC
import Test.Tasty.TH

unit_foo = undefined
