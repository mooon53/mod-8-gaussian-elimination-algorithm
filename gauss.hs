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

-- Bubble sort for 1 iteration
sortV xs = map fst sorted :< (snd (last sorted))
 where
   lefts  = head xs :> map snd (init sorted)
   rights = tail xs
   sorted = zipWith compareSwapL lefts rights

-- Compare and swap
compareSwapL a b = if a < b then (a,b)
                            else (b,a)

sortVL xs = map fst sorted :< (snd (last sorted))
 where
   lefts  = head xs :> map snd (init sorted)
   rights = tail xs
   sorted = zipWith compareSwapL (lazyV lefts) rights

-- Helper function to extract the values from MaybeValue
extractValues :: Vec 8 (Maybe Value) -> Vec 8 Value
extractValues = map (\v -> case v of { Just val -> val; _ -> 0 })


-- Check if a value is zero
isZero :: MaybeValue -> Bool
isZero (Just val) = val == 0
isZero _          = True

-- Check if a row consists of only zeros
isZeroRow :: Row -> Bool
isZeroRow = all isZero

-- Check each row in the matrix for being a zero row
allZeroRows :: Matrix -> Vec 8 Bool
allZeroRows = map isZeroRow


-- Get the value at a specific row and column in the matrix
getValue :: Matrix -> Index 8 -> Index 8 -> MaybeValue
getValue matrix rowIdx colIdx = matrix !! rowIdx !! colIdx


-- Check if the determinant of the matrix is zero
isDeterminantZero :: Matrix -> Bool
isDeterminantZero matrix =
  let indices = indicesI :: Vec 8 (Index 8)
      determinant = sum (imap (\colIdx col ->
                                let a = getValue matrix 0 colIdx
                                    b = getValue matrix 0 (colIdx + 1)
                                    c = getValue matrix 1 colIdx
                                    d = getValue matrix 1 (colIdx + 1)
                                in case (a, b, c, d) of
                                     (Just aVal, Just bVal, Just cVal, Just dVal) ->
                                       aVal * dVal - bVal * cVal
                                     _ -> 0
                              ) indices)
  in determinant == 0
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

