type Number = Double
type Vector = [Number]
type Row    = [Number]
type Matrix = [Row]
--In other words, vectors are simply represented as lists of numbers and matrices are lists of rows, which in turn are lists of numbers.

-- A matrix equation can then be written as

-- mapMatrix a x == b
-- where the mapMatrix function multiplies a matrix with a vector

mapMatrix :: Matrix -> Vector -> Vector
mapMatrix rows v = [sum (zipWith (*) row v) | row <- rows]

gauss :: Matrix -> Vector -> Vector
gauss a b = x
    where
    b' = map (\y -> [y]) b
    a' = zipWith (++) a b'    -- combine with right-hand side

    x  = resubstitute $ triangular a'
-- Then, the algorithm transforms the matrix into triangular form and performs the substitutions to obtain the unknown vector \vec x.

-- First phase – triangular form
-- For the first phase, we take the first row and subtract a suitable multiple of it from the other rows so that the first column of coefficients will become zero. We can drop this column of zeroes and continue the algorithm on a smaller matrix.

triangular :: Matrix -> Matrix
triangular [] = []
triangular m  = row:(triangular rows')
    where
    (row:rows) = rotatePivot m    -- see discussion below
    rows' = map f rows
    f bs
        | (head bs) == 0 = drop 1 bs
        | otherwise      = drop 1 $ zipWith (-) (map (*c) bs) row
        where 
        c = (head row)/(head bs)    -- suitable multiple
-- Note that we represent the triangular matrix with rows of decreasing length, this makes resubstitution simpler later.

-- ghci> triangular exampleA
--   [[1.0,1.0,0.0],[1.0,1.0],[-2.0]]
-- Of course, there is a problem if the first coefficient in the first row is already zero, because then we cannot eliminate the other coefficients. However, in this case, we simply look for another row that has a non-zero coefficient in the first column, this is the job of the rotatePivot function. (If no such row exists, then the system of equations does not have a unique solution, we will simply ignore this case.)

rotatePivot :: Matrix -> Matrix
rotatePivot (row:rows)
    | (head row) /= 0 = (row:rows)
    | otherwise       = rotatePivot (rows ++ [row])
-- Second phase – resubstitution
-- When the matrix is in triangular form, the equations are very easy to solve. We simply start with the equation that contains only one unknown and
-- work back from there.

-- When working with lists in Haskell, it’s easiest to start from the head of the list. To formulate the resubstitution, we therefore apply a few 
--administrative steps to the output of the triangular. Namely, we first reverse the columns to have the last unknown in the head column, and we 
--reverse the rows as well to start with the equation that has only one unknown. Afterwards, we have to undo the column reversion in the result.

resubstitute :: Matrix -> Vector
resubstitute = reverse . resubstitute' . reverse . map reverse
--Resubstitution itself is not very difficult. We simply calculate the solution from the first equation with only one unknown and substitute this
-- result into the other equations, eliminating one variable.

resubstitute' :: Matrix -> Vector
resubstitute' [] = []
resubstitute' (row:rows) = x:(resubstitute' rows')
    where
    x     = (head row)/(last row)
    rows' = map substituteUnknown rows
    substituteUnknown (a1:(a2:as')) = ((a1-x*a2):as')
-- Examples
-- Have fun solving linear equations in Haskell.

-- ghci> let x = gauss exampleA exampleb
-- ghci> x
--   [1.5,0.5,2.5]

-- ghci> mapMatrix exampleA x == exampleb
--   True

example1, example2, example3 :: Matrix
example1 = [[1,1],[1,2]]
example2 = [[1,1,1,3],[1,2,3,7],[3,4,6,31]]
example3 = [[0,0,1],[1,0,0],[0,1,0]]

-- ghci> gauss example1 [2,3]
--   [1.0,1.0]

-- ghci> gauss example2 [1,0,0,1]
--   [8.881784197001252e-16,6.9999999999999964,-1.9999999999999991]

-- ghci> gauss example3 [1,1,1]
--   [1.0,1.0,1.0]