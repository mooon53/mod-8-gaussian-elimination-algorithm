import Clash.Prelude
import Test.HUnit

type Value = Maybe (Signed 8)
type Row = Vector 8 Value
type Matrix = Vector 8 Row

data RowOperation = Add Int Int
                    |Mul Num Int
                    |Swap Int Int

addMaybe :: Value -> Value -> Value
addMaybe Nothing a = a
addMaybe a Nothing = a
addMaybe a b = a + b

-- Test cases for addMaybe function
addMaybeTests =
  TestList
    [ TestCase $
        assertEqual "addMaybe test 1"
          (addMaybe (Just 5) (Just 3))
          (Just 8)
    , TestCase $
        assertEqual "addMaybe test 2"
          (addMaybde (Just (-5)) (Just 3))
          (Just (-2))
    , TestCase $
        assertEqual "addMaybe test 3"
          (addMaybe (Just 5) Nothing)
          (Just 5)
    , TestCase $
        assertEqual "addMaybe test 4"
          (addMaybe Nothing (Just 3))
          (Just 3)
    , TestCase $
        assertEqual "addMaybe test 5"
          (addMaybe Nothing Nothing)
          Nothing
    ]

-- Run all the test cases
runTests :: IO ()
runTests = do
  putStrLn "Running addMaybe tests:"
  runTestTT addMaybeTests
