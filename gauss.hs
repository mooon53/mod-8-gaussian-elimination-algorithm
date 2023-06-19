{-# LANGUAGE NoImplicitPrelude #-}
import Clash.Prelude

--import Prelude hiding (map, (!!), zipWith)
type Clk = Clock System
type Rst = Reset System
type Sig = Signal System

type Value = Signed 8
type MaybeValue = Maybe Value
type Row = Vec 8 MaybeValue
type Matrix = Vec 8 Row


--add raw Rows or get vectors from given indices
data RowOperation = Add Int Int
                    |Mul Value Int
                    |Swap Int Int 
                    deriving (Eq, Show)

--operations on Maybe datatype
addMaybe :: MaybeValue -> MaybeValue -> MaybeValue
addMaybe Nothing a = Nothing
addMaybe a Nothing = Nothing
addMaybe (Just a)  (Just b) = Just (a + b)

multMaybe :: Value -> MaybeValue -> MaybeValue
multMaybe a Nothing = Nothing
multMaybe a  (Just b) = Just (a * b)


--define elementary row operations using mealy
perform :: Matrix -> RowOperation -> (Matrix, Matrix)

--add base cases
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


meperform :: (HiddenClockResetEnable dom) => Signal dom RowOperation -> Signal dom Matrix
meperform i = mealy perform (vec :> vec :>vec :>vec :>vec :>vec :>vec :>vec :>Nil) i
        where vec = Just 1:> Just 2:> Just 3:> Just 4:> Just 5:> Just 6:>Just 7:>Just 8:>Nil

topEntity :: Clk -> Rst -> Sig RowOperation -> Sig Matrix
topEntity clk rst i = withClockResetEnable clk rst enableGen meperform i


--replace :: (KnownNat n, Enum i) => i -> a -> Vec n a -> Vec n a

--let vec = Just 1:> Just 2:> Just 3:> Just 4:> Just 5:> Just 6:>Just 7:>Just 8:>Nil

--let matr = vec :> vec :>vec :>vec :>vec :>vec :>vec :>vec :>Nil

