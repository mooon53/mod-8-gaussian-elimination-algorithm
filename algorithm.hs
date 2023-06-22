{-# LANGUAGE NoImplicitPrelude #-}
import Clash.Prelude


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



-- let vec = Just 1:> Just 2:> Just 3:> Just 4:> Just 5:> Just 6:>Just 7:>Just 8:>Nil
-- let vec1 = Just 0:> Just 2:> Just 3:> Just 4:> Just 5:> Just 6:>Just 7:>Just 8:>Nil
-- let matr = vec1 :> vec :>vec :>vec :>vec :>vec :>vec :>vec :>Nil

