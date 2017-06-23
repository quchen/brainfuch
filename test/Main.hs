module Main (main) where



import Brainfuch

import Test.Tasty
import Test.Tasty.HUnit



main :: IO ()
main = (defaultMain . testGroup "Example programs")
    [ testCase "Super simple" (runTestProgram "superSimple.bf" "" "A")
    , testCase "Hello World" (runTestProgram "helloworld.bf" "" "Hello World!\n")

    -- The rot13 program just keeps running indefinitely, so an ending input stream is unexpected
    , testCase "Rot 13" (runTestProgram "rot13.bf" "Hello World!" "Uryyb Jbeyq!<unexpected end of input>")
    ]

runTestProgram :: FilePath -> String -> String -> Assertion
runTestProgram fp stdin expected = do
    let path = "bfExamplePrograms/" ++ fp
    source <- readFile path
    let actual = runString stdin source
    assertEqual "" expected actual
