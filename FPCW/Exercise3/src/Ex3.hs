module Ex3 where

--required for all Qs:
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
find :: String -> Dict -> Maybe Float
find s [] = Nothing
find s ((t,f):d)
  | s==t       =  Just f
  | otherwise  =  find s d

-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (8 marks)
-- implement the following function (which may have runtime errors):
eval :: Dict -> CExpr -> Float
eval _ (Value x) = x
eval dict (Variable s) = case find s dict of
  Just x -> x
  Nothing -> error ("Variable not found: " ++ s)
eval dict (Div e1 e2) = eval dict e1 / eval dict e2
eval dict (MulBy e1 e2) = eval dict e1 * eval dict e2
eval dict (AbsVal e) = abs (eval dict e)
eval dict (Not e) = if eval dict e == 0.0 then 1.0 else 0.0
eval dict (Eql e1 e2) = if eval dict e1 == eval dict e2 then 1.0 else 0.0
eval dict (IsNil e) = if eval dict e == 0.0 then 1.0 else 0.0

-- Q2 (8 marks)
-- implement the following function (which always returns a value):
meval :: Dict -> CExpr -> Maybe Float
meval _ (Value x) = Just x
meval dict (Variable s) = find s dict
meval dict (Div e1 e2) = case (meval dict e1, meval dict e2) of
  (Just x1, Just x2) -> if x2 /= 0.0 then Just (x1 / x2) else Nothing
  _ -> Nothing
meval dict (MulBy e1 e2) = case (meval dict e1, meval dict e2) of
  (Just x1, Just x2) -> Just (x1 * x2)
  _ -> Nothing
meval dict (AbsVal e) = fmap abs (meval dict e)
meval dict (Not e) = fmap (\x -> if x == 0.0 then 1.0 else 0.0) (meval dict e)
meval dict (Eql e1 e2) = case (meval dict e1, meval dict e2) of
  (Just x1, Just x2) -> Just (if x1 == x2 then 1.0 else 0.0)
  _ -> Nothing
meval dict (IsNil e) = fmap (\x -> if x == 0.0 then 1.0 else 0.0) (meval dict e)

-- Q3 (4 marks)
-- Laws of Arithmetic for this question:
--    x + 0 = x
--    0 + x = x
--    x - 0 = x
--    x - x = 0
--    x * 0 = 0
--    1 * x = x
-- The following function should implement the two laws applicable
-- for *your* CExpr datatype.
simp :: CExpr -> CExpr
simp (MulBy (Value 0.0) _) = Value 0.0
simp (MulBy _ (Value 0.0)) = Value 0.0
simp (MulBy (Value 1.0) e) = e
simp (MulBy e (Value 1.0)) = e
simp e = e

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...

