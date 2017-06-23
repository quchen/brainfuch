{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import           Data.Text      (Text)
import qualified Data.Text.IO   as T
import qualified Data.Text.Lazy as TL

import Brainfuch

import Test.Tasty
import Test.Tasty.HUnit



main :: IO ()
main = (defaultMain . testGroup "Example programs")
    [ testCase "Super simple" (runTestProgram "superSimple.bf" "" "A")
    , testCase "Hello World" (runTestProgram "helloworld.bf" "" "Hello World!\n")

    -- The rot13 program just keeps running indefinitely, so an ending input stream is unexpected
    , testCase "Rot 13" (runTestProgram "rot13.bf" "Hello World!\n" "Uryyb Jbeyq!\n<unexpected end of input>")
    ]

runTestProgram :: FilePath -> Text -> TL.Text -> Assertion
runTestProgram fp stdin expected = do
    let path = "bfExamplePrograms/" ++ fp
    source <- T.readFile path
    let actual = runText stdin source
    assertEqual "" expected actual
