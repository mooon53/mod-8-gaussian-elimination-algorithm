import Clash.Prelude

type Value = Maybe (Signed 8)
type Row = Vector 8 Value
type Matrix = [Row]


--add raw Rows or get vectors from given indices
type RowOperation = Add Int Int
                    |Mul Num Int
                    |Swap Int Int

--define elementary row operations using mealy
perform :: Matrix -> RowOperation -> a -> b -> Matrix

--add base cases
perform matr Add vec1 vec2 = new_matr 
                            where
                                new_matr = replace (matr !! vec1) new_vec matr
                                new_vec = zipWith (+) (matr !! vec1) (matr !! vec2)
perform matr Mul a vec = new_matr
                        where
                            new_matr = replace (matr !! vec) new_vec matr
                            new_vec = map (*a) (matr !! vec)
perform matr Swap vec1 vec2 = 

