import Clash.Prelude
import Test.HUnit

type Value = Signed 8
type MaybeValue = Maybe Value
type Row = Vec 8 MaybeValue
type Matrix = Vec 8 Row

--create echelon form
echelon :: Matrix -> Matrix
echelon matr = (head new_matr) :> (echelon (other_rows:>noting_vec))
    where
        new_matr = reorder matr
        noting_vec = Nothing :> Nothing :> Nothing :> Nothing :> Nothing :> Nothing :> Nothing :> Nothing :> Nil
        other_rows = map f (tail new_matr)
        f xs
            | (head (head xs)) == Just 0 = (replace 0 Nothing (head (head xs)))
            | otherwise = zipWith (-) (map (*c) xs) (head new_matr)
            where 
                c = (head (head new_matr))/(head (head xs))


reorder :: Matrix -> Matrix
reorder m 
        | (head (head m)) /= Just 0 = m
        | otherwise = reorder ((tail m):<(head m))



-- Test cases for reorder function
reorderTests =
  TestList
    [ TestCase $
        assertEqual "reorder test 1"
          (reorder ((Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Nil) :> (Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Nil) :> (Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Nil) :> (Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Nil) :> (Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Nil) :> (Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Nil) :> (Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Nil) :> (Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Nil) :> Nil))
          ((Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Nil) :> (Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Nil) :> (Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Nil) :> (Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Nil) :> (Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Nil) :> (Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Nil) :> (Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Nil) :> (Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Nil) :> Nil)
    , TestCase $
        assertEqual "reorder test 2"
          (reorder ((Just 0 :> Just 0 :> Just 0 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Nil) :> (Just 0 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Nil) :> (Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Nil) :> (Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Nil) :> (Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Nil) :> (Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Nil) :> (Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Nil) :> (Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Nil) :> Nil))
          ((Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Nil) :> (Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Nil) :> (Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Nil) :> (Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Nil) :> (Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Nil) :> (Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Nil) :> (Just 0 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Nil) :> (Just 0 :> Just 0 :> Just 0 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Nil) :> Nil)
    , TestCase $
        assertEqual "reorder test 3"
          (reorder ((Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Nil) :> (Just 0 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Nil) :> (Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Nil) :> (Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Nil) :> (Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Nil) :> (Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Nil) :> (Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Nil) :> (Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Nil) :> Nil))
          ((Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Nil) :> (Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Nil) :> (Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Nil) :> (Just 5 :> Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Nil) :> (Just 6 :> Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Nil) :> (Just 7 :> Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Nil) :> (Just 8 :> Just 1 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Nil) :> (Just 0 :> Just 2 :> Just 3 :> Just 4 :> Just 5 :> Just 6 :> Just 7 :> Just 8 :> Nil) :> Nil)
    ]

--TODO: echelon form testings


runTests = do
  putStrLn "Running reorder tests:"
  runTestTT reorderTests

  putStrLn "Running performMul tests:"
  runTestTT performMulTests

   putStrLn "Running performSwap tests:"
  runTestTT performSwapTests

-- vec = Just 0:> Just 0:> Just 0:> Just 1:> Just 2:> Just 3:> Just 4:> Just 5:>Nil
-- vec0 = Just 0:> Just 2:> Just 3:> Just 4:> Just 5:> Just 6:> Just 7:> Just 8:>Nil
-- vec1 = Just 1:> Just 2:> Just 3:> Just 4:> Just 5:> Just 6:> Just 7:> Just 8:>Nil
-- vec2 = Just 2:> Just 3:> Just 4:> Just 5:> Just 6:> Just 7:> Just 8:> Just 1:> Nil
-- vec3 = Just 3:> Just 4:> Just 5:> Just 6:> Just 7:> Just 8:> Just 1:> Just 2:>Nil
-- vec4 = Just 4:> Just 5:> Just 6:> Just 7:> Just 8:> Just 1:> Just 2:> Just 3:>Nil
-- vec5 = Just 5:> Just 6:> Just 7:> Just 8:> Just 1:> Just 2:> Just 3:> Just 4:> Nil
-- vec6 = Just 6:> Just 7:> Just 8:> Just 1:> Just 2:> Just 3:> Just 4:> Just 5:>Nil
-- vec7 = Just 7:> Just 8:> Just 1:> Just 2:> Just 3:> Just 4:> Just 5:> Just 6:>Nil
-- vec8 = Just 8:> Just 1:> Just 2:> Just 3:> Just 4:> Just 5:> Just 6:> Just 7:>Nil

-- matr = vec1 :> vec2 :>vec3 :>vec4 :>vec5 :>vec6 :>vec7 :>vec8 :>Nil