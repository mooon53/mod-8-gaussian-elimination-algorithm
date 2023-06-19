{-# LANGUAGE NoImplicitPrelude #-}
import Clash.Prelude

type Value = Signed 8
type MaybeValue = Maybe Value
type Row = Vec 8 MaybeValue


--add raw Rows or get vectors from given indices
data RowOperation = Add Row Row
                    |Mul Row Value
                    |Div Row Value
                    deriving (Eq, Show)

--operations on Maybe datatype
addMaybe :: MaybeValue -> MaybeValue -> MaybeValue
addMaybe Nothing a = Nothing
addMaybe a Nothing = Nothing
addMaybe (Just a)  (Just b) = Just (a + b)

multMaybe :: Value -> MaybeValue -> MaybeValue
multMaybe a Nothing = Nothing
multMaybe a  (Just b) = Just (a * b)

divMaybe :: Value -> MaybeValue -> MaybeValue
divMaybe a Nothing = Nothing
divMaybe a  (Just b) = Just (a * b)


--define elementary row operations using mealy
perform :: RowOperation -> Row

--add base cases
perform (Add row1 row2) = res 
                        where
                            res = zipWith addMaybe row1 row2
                                
perform (Mul row1 con) = res
                        where
                            res = map (multMaybe con) row1
                            
perform (Div row1 con) = res
                        where 
                            res = map (divMaybe con) row1

topEntity :: RowOperation -> Row
topEntity = perform
--replace :: (KnownNat n, Enum i) => i -> a -> Vec n a -> Vec n a

--let vec = Just 1:> Just 2:> Just 3:> Just 4:> Just 5:> Just 6:>Just 7:>Just 8:>Nil

--let matr = vec :> vec :>vec :>vec :>vec :>vec :>vec :>vec :>Nil


 

