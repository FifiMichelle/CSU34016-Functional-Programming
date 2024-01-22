module Ex4 where

--required for Q1
data CExpr -- the expression datatype
  = Value Float -- floating-point value
  | Variable String -- variable/identifier name
  | Div CExpr CExpr -- divide first by second
  | MulBy CExpr CExpr -- multiplies both
  | AbsVal CExpr -- absolute value
  -- the following are boolean expressions (using numbers)
  -- the number 0.0 represents False, all others represent True.
  | Not CExpr -- logical not
  | Eql CExpr CExpr -- True if both are the same
  | IsNil CExpr -- True if numeric value is zero
  deriving (Eq,Ord,Show)

type Dict = [(String,Float)]
insert :: String -> Float -> Dict -> Dict
insert s f d = (s,f):d
find :: MonadFail m => String -> Dict -> m Float
find s [] = fail (s++" not found")
find s ((t,f):d)
  | s==t       =  return f
  | otherwise  =  find s d

-- required for Q2
x `incfst` _  =  x + 1
_ `incsnd` y  =  1 + y
type Thing = ([Bool],Float)

-- required for all Qs:

-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (8 marks)
mdeval :: MonadFail m => Dict -> CExpr -> m Float
mdeval dict (Value v) = return v
mdeval dict (Variable x) = find x dict
mdeval dict (Div e1 e2) = do
    v1 <- mdeval dict e1
    v2 <- mdeval dict e2
    if v2 == 0 then fail "Error" else return (v1 / v2)
mdeval dict (MulBy e1 e2) = do
    v1 <- mdeval dict e1
    v2 <- mdeval dict e2
    return (v1 * v2)
mdeval dict (AbsVal e) = fmap abs (mdeval dict e)
mdeval dict (Not e) = fmap (\v -> if v == 0 then 1 else 0) (mdeval dict e)
mdeval dict (Eql e1 e2) = do
    v1 <- mdeval dict e1
    v2 <- mdeval dict e2
    return (if v1 == v2 then 1 else 0)
mdeval dict (IsNil e) = fmap (\v -> if v == 0 then 1 else 0) (mdeval dict e)

-- Q2 (8 marks)
-- Consider the following four recursive pattern definitions:
len :: Int -> [Int] -> Int
len z []     = z
len z (x:xs) = len (z `incfst` x) xs
sumup :: Int -> [Int] -> Int
sumup sbase []     = sbase
sumup sbase (n:ns) = sumup (sbase + n) ns
prod :: Int -> [Int] -> Int
prod mbase []     = mbase
prod mbase (n:ns) = prod (mbase * n) ns
cat :: [Thing] -> [[Thing]] -> [Thing]
cat pfx []     = pfx
cat pfx (xs:xss) = cat (pfx ++ xs) xss

-- They all have the same abstract pattern,
-- as captured by the following Higher Order Function (HOF):
foldL z _ [] = z
foldL z op (x:xs) = foldL (z `op` x) op xs

-- We can gather the `z` and `opr` arguments into a tuple: (op,z)
-- which allows us to construct a call to foldL as:
dofold (op,z) = foldL z op

-- Your task is to complete the tuples below,
-- so that `dofold` can be used to implement the fns. above.

lenTuple :: (Int -> Int -> Int, Int)
lenTuple = (incfst, 0)

sumupTuple :: (Int -> Int -> Int, Int)
sumupTuple = ((+), 0)

prodTuple :: (Int -> Int -> Int, Int)
prodTuple = ((*), 1)

catTuple :: ([Thing] -> [Thing] -> [Thing], [Thing])
catTuple = ((++), [])

-- Q3 (11 marks)
sub = subtract -- shorter!
ops = [(*20),(+27),(*19),(+29),(+21),(20-),(23-),(21-),(26-),(+20),(*29)]

-- (!) This question requires modifying Main.hs
-- See, and/or compile and run Main.hs for further details

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...

