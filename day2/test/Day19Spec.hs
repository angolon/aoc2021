{-# LANGUAGE TemplateHaskell #-}

module Day19Spec where

import Day19
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.TH

unit_allOrientations =
  length allFacingOrientations @?= 24
