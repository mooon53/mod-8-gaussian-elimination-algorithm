import Clash.Prelude
import Test.HUnit

type Value = Maybe (Signed 8)
type Row = Vec 8 Value
type Matrix = Vec 8 Row

data RowOperation = Add Int Int
                    |Mul Int Int
                    |Swap Int Int

addMaybe :: Value -> Value -> Value
addMaybe Nothing a = a
addMaybe a Nothing = a
addMaybe (Just a) (Just b) = Just (a + b)

multMaybe :: Value -> MaybeValue -> MaybeValue
multMaybe a Nothing = Nothing
multMaybe a  (Just b) = Just (a * b)

-- Test cases for addMaybe function
addMaybeTests =
  TestList
    [ TestCase $
        assertEqual "addMaybe test 1"
          (addMaybe (Just 5) (Just 3))
          (Just 8)
    , TestCase $
        assertEqual "addMaybe test 2"
          (addMaybe (Just (-5)) (Just 3))
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

-- Test cases for multMaybe function
multMaybeTests =
  TestList
    [ TestCase $
        assertEqual "multMaybe test 1"
          (multMaybe (5) (Just 3))
          (Just 15)
    , TestCase $
        assertEqual "multMaybe test 2"
          (multMaybe (-5) (Just 3))
          (Just (-15))
    , TestCase $
        assertEqual "multMaybe test 3"
          (multMaybe (0) Nothing)
          (Nothing)
    , TestCase $
        assertEqual "multMaybe test 4"
          (multMaybe 3 (Nothing))
          (Nothing)
    ]

-- Run all the test cases
runTests = do
  putStrLn "Running addMaybe tests:"
  runTestTT addMaybeTests

  putStrLn "Running multMaybe tests:"
  runTestTT multMaybeTests