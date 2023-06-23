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


perform matr (Add id1 id2) = (new_matr, new_matr) 
                        where
                            new_matr = replace id1 new_vec matr
                            new_vec = zipWith addMaybe (matr !! id1) (matr !! id2)
                                
perform matr (Mul a id) = (new_matr, new_matr)
                        where
                            new_matr = replace id new_vec matr
                            new_vec = map (multMaybe a) (matr !! id)
                            
perform matr (Swap id1 id2) = (new_matr, new_matr)
                        where 
                            row1 = matr !! id1
                            row2 = matr !! id2
                            new_matr = replace id1 row2 (replace id2 row1 matr)



-- Test cases for perform matr Add function
performAddTests =
  TestList
    [ TestCase $
        assertEqual "performAdd test"
          perform ((Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Nil) :> (Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Nil) :> (Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Nil) :> (Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Nil) :> (Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Nil) :> (Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Nil) :> (Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Nil) :> (Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Nil) :> Nil) (Add 0 1)
          ((Just 3 :> Just 5 :> Just 7 :> Just 9 :> Just 11 :> Just 13 :> Just 15 :> Just 9 :> Nil) :> (Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Nil) :> (Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Nil) :> (Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Nil) :> (Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Nil) :> (Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Nil) :> (Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Nil) :> (Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Nil) :> Nil)
    ]

-- Test cases for perform matr Mul function
performMulTests =
  TestList
    [ TestCase $
        assertEqual "performMul test 1"
          perform ((Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Nil) :> (Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Nil) :> (Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Nil) :> (Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Nil) :> (Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Nil) :> (Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Nil) :> (Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Nil) :> (Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Nil) :> Nil) (Mul 0 0)
          ((Just 0 :> Just 0 :> Just 0 :> Just 0 :> Just 0 :> Just 0 :> Just 0 :> Just 0 :> Nil) :> (Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Nil) :> (Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Nil) :> (Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Nil) :> (Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Nil) :> (Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Nil) :> (Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Nil) :> (Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Nil) :> Nil)
    , TestCase $
        assertEqual "performMul test 2"
          perform ((Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Nil) :> (Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Nil) :> (Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Nil) :> (Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Nil) :> (Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Nil) :> (Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Nil) :> (Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Nil) :> (Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Nil) :> Nil) (Mul 2 0)
          ((Just 2 :> Just 4 :> Just 6 :> Just 8 :> Just 10 :> Just 12 :> Just 14 :> Just 16 :> Nil) :> (Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Nil) :> (Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Nil) :> (Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Nil) :> (Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Nil) :> (Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Nil) :> (Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Nil) :> (Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Nil) :> Nil)
    , TestCase $
        assertEqual "performMul test 3"
          perform ((Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Nil) :> (Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Nil) :> (Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Nil) :> (Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Nil) :> (Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Nil) :> (Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Nil) :> (Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Nil) :> (Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Nil) :> Nil) (Mul (-2) 0)
          ((Just (-2) :> Just (-4) :> Just (-6) :> Just (-8) :> Just (-10) :> Just (-12) :> Just (-14) :> Just (-16) :> Nil) :> (Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Nil) :> (Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Nil) :> (Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Nil) :> (Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Nil) :> (Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Nil) :> (Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Nil) :> (Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Nil) :> Nil)
    ]

-- Test cases for perform matr Swap function
performSwapTests =
  TestList
    [ TestCase $
        assertEqual "performSwap test"
          perform ((Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Nil) :> (Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Nil) :> (Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Nil) :> (Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Nil) :> (Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Nil) :> (Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Nil) :> (Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Nil) :> (Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Nil) :> Nil) (Swap 0 1)
          ((Just 2:> Just 3:> Just 4:> Just 5:> Just 6:> Just 7:> Just 8:> Just 1:> Nil) :> (Just 1:> Just 2:> Just 3:> Just 4:> Just 5:> Just 6:> Just 7:> Just 8:>Nil) :> (Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Nil) :> (Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Nil) :> (Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Nil) :> (Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Nil) :> (Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Nil) :> (Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Nil) :> Nil)
    ]

-- Run all the test cases
runTests = do
  putStrLn "Running performAdd tests:"
  runTestTT performAddTests

  putStrLn "Running performMul tests:"
  runTestTT performMulTests

   putStrLn "Running performSwap tests:"
  runTestTT performSwapTests


-- vec1 = Just 1:> Just 2:> Just 3:> Just 4:> Just 5:> Just 6:> Just 7:> Just 8:>Nil
-- vec2 = Just 2:> Just 3:> Just 4:> Just 5:> Just 6:> Just 7:> Just 8:> Just 1:> Nil
-- vec3 = Just 3:> Just 4:> Just 5:> Just 6:> Just 7:> Just 8:> Just 1:> Just 2:>Nil
-- vec4 = Just 4:> Just 5:> Just 6:> Just 7:> Just 8:> Just 1:> Just 2:> Just 3:>Nil
-- vec5 = Just 5:> Just 6:> Just 7:> Just 8:> Just 1:> Just 2:> Just 3:> Just 4:> Nil
-- vec6 = Just 6:> Just 7:> Just 8:> Just 1:> Just 2:> Just 3:> Just 4:> Just 5:>Nil
-- vec7 = Just 7:> Just 8:> Just 1:> Just 2:> Just 3:> Just 4:> Just 5:> Just 6:>Nil
-- vec8 = Just 8:> Just 1:> Just 2:> Just 3:> Just 4:> Just 5:> Just 6:> Just 7:>Nil
-- matr = vec1 :> vec2 :>vec3 :>vec4 :>vec5 :>vec6 :>vec7 :>vec8 :>Nil