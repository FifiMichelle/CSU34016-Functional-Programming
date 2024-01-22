module Ex2 where

add :: Int -> Int -> Int
add x y = (x+y) `mod` 65563

mul :: Int -> Int -> Int
mul x y
  | p == 0    = 1
  | otherwise = p
  where p = (x*y) `mod` 65563

-- DON'T RENAME THE SPECIFIED FUNCTIONS (f1..fN)
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (3 marks)
f1 :: [a] -> [a]
f1 xs
  | length xs < 120 = []  -- If the list is shorter than 120 elements, return an empty list.
  | otherwise = every120th xs 1
  where
    every120th [] _ = []
    every120th (x:xs) n
      | n `mod` 120 == 0 = x : every120th xs (n + 1)
      | otherwise = every120th xs (n + 1)

-- Q2 (3 marks)
f2 :: [Int] -> Int
f2 ns
  | length ns < 247 = 0  -- If the list is shorter than 247 elements, return 0.
  | otherwise = sumEvery247th ns 1
  where
    sumEvery247th [] _ = 0
    sumEvery247th (x:xs) n
      | n `mod` 247 == 0 = x + sumEvery247th xs (n + 1)
      | otherwise = sumEvery247th xs (n + 1)

-- Q3 (4 marks)
f3 :: [Int] -> Int
f3 ns
  | length ns < 304 = 1  -- If the list is shorter than 304 elements, return 1.
  | otherwise = multiplyEvery304th ns 1
  where
    multiplyEvery304th [] _ = 1
    multiplyEvery304th (x:xs) n
      | n `mod` 304 == 0 = x * multiplyEvery304th xs (n + 1)
      | otherwise = multiplyEvery304th xs (n + 1)
      

-- Q4 (8 marks)
f4 :: [Maybe Int] -> (Int, [Maybe Int])
f4 [] = (0, [])
f4 xs' = case skipUntilOpCode xs' of
    [] -> (0, [])
    (x:xs) -> processOpCode x xs
    where
      processOpCode x xs =
        case x of
          Just 30 -> let (result, rest) = opFixed Add 3 xs
                     in if rest == xs then (0, []) else (result, rest)
          Just 13 -> let (result, rest) = skipN Add 5 xs
                     in if rest == xs then (0, []) else (result, rest)
          Just 52 -> let (result, rest) = fixedNum Add 5 5 xs
                     in if rest == xs then (0, []) else (result, rest)
          Just 26 -> let (result, rest) = stopTerm Add 3 xs
                     in if rest == xs then (0, []) else (result, rest)
          Just 79 -> let (result, rest) = stopSkip Add 4 xs
                     in if rest == xs then (0, []) else (result, rest)
          Just 10 -> let (result, rest) = stopFixedN Add 3 0 xs
                     in if rest == xs then (0, []) else (result, rest)
          Just 27 -> let (result, rest) = opFixed Mul 6 xs
                     in if rest == xs then (1, []) else (result, rest)
          Just 86 -> let (result, rest) = skipN Mul 6 xs
                     in if rest == xs then (1, []) else (result, rest)
          Just 20 -> let (result, rest) = fixedNum Mul 6 4 xs
                     in if rest == xs then (1, []) else (result, rest)
          Just 72 -> let (result, rest) = stopTerm Mul 4 xs
                     in if rest == xs then (1, []) else (result, rest)
          Just 11 -> let (result, rest) = stopSkip Mul 4 xs
                     in if rest == xs then (1, []) else (result, rest)
          Just 93 -> let (result, rest) = stopFixedN Mul 3 0 xs
                     in if rest == xs then (1, []) else (result, rest)
          _ -> (maybe 0 id x, xs)
          
-- Operation Table (See Exercise2 description on BB)
--    ___________________________________________
--    | opcode | operation | operands | Nothing |
--    -------------------------------------------
--    |   30   |    add    | fixed 3  | term    |
--    |   13   |    add    | fixed 5  | skip    |
--    |   52   |    add    | fixed 5  | 5       |
--    |   26   |    add    | stop@ 3  | term    |
--    |   79   |    add    | stop@ 4  | skip    |
--    |   10   |    add    | stop@ 3  | 0       |
--    |   27   |    mul    | fixed 6  | term    |
--    |   86   |    mul    | fixed 6  | skip    |
--    |   20   |    mul    | fixed 6  | 4       |
--    |   72   |    mul    | stop@ 4  | term    |
--    |   11   |    mul    | stop@ 4  | skip    |
--    |   93   |    mul    | stop@ 3  | 0       |
--    -------------------------------------------

-- Q5 (2 marks)
f5 :: [Maybe Int] -> [Int]
f5 [] = []
f5 xs = case f4 xs of
  (result, rest) -> result : f5 rest

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...

data Operation = Add | Mul

applyOp :: Operation -> Int -> Int -> Int
applyOp Add x y = add x y
applyOp Mul x y = mul x y

opFixed :: Operation -> Int -> [Maybe Int] -> (Int, [Maybe Int])
opFixed op 0 xs =
  case op of
    Add -> (0, xs)
    Mul -> (1, xs)
opFixed op _ [] = (0, [])
opFixed op c (Just x:xs) =
  let (result, rest) = opFixed op (c - 1) xs
  in (applyOp op x result, rest)
opFixed op _ (Nothing:xs) =
  case op of
    Add -> (0, xs)
    Mul -> (1, xs)
    
fixedNum :: Operation -> Int ->  Int ->  [Maybe Int] -> (Int, [Maybe Int])
fixedNum op 0 n xs = case op of
                    Add -> (0, xs)
                    Mul -> (1, xs)
fixedNum op _ n [] = (0, [])
fixedNum op c n (Just x:xs) =
  let (result, rest) = fixedNum op (c - 1) n xs
  in (applyOp op x result, rest)
fixedNum op c n (Nothing:xs) =
  let (result, rest) = fixedNum op c n xs
  in (applyOp op n result, rest)
  
skipN :: Operation -> Int -> [Maybe Int] -> (Int, [Maybe Int])
skipN op 0 xs = case op of
                              Add -> (0, xs)
                              Mul -> (1, xs)
skipN op _ [] = (0, [])
skipN op c (Just x:xs) =
    let (result, rest) = skipN op (c - 1) xs
    in (applyOp op x result, rest)
skipN op c (Nothing:xs) = skipN op c xs

stopTerm :: Operation -> Int -> [Maybe Int] -> (Int, [Maybe Int])
stopTerm op _ [] = (0, [])
stopTerm op stopNum (Just x:xs)
  | x == stopNum = case op of
              Add -> (0, xs)
              Mul -> (1, xs)
  | otherwise =
     let (result, rest) = stopTerm op stopNum xs
     in (applyOp op x result, rest)
stopTerm op _ (Nothing:xs) =
      case op of
        Add -> (0, xs)
        Mul -> (1, xs)

stopSkip :: Operation -> Int -> [Maybe Int] -> (Int, [Maybe Int])
stopSkip op _ [] = (0, [])
stopSkip op stopNum (Just x:xs)
          | x == stopNum = case op of
                      Add -> (0, xs)
                      Mul -> (1, xs)
          | otherwise =
             let (result, rest) = stopSkip op stopNum xs
             in (applyOp op x result, rest)
stopSkip op stopNum (Nothing:xs) = stopSkip op stopNum xs

stopFixedN:: Operation -> Int ->  Int ->  [Maybe Int] -> (Int, [Maybe Int])
stopFixedN op _ n [] = (0, [])
stopFixedN op stop n (Just x:xs)
            | x == stop = case op of
                            Add -> (0, xs)
                            Mul -> (1, xs)
            | otherwise =
              let (result, rest) = stopFixedN op stop n xs
              in (applyOp op x result, rest)
stopFixedN op stop n (Nothing:xs) =
  let (result, rest) = stopFixedN op stop n xs
  in (applyOp op n result, rest)


skipUntilOpCode :: [Maybe Int] -> [Maybe Int]
skipUntilOpCode (x:xs)
    | x `elem` [Just 30, Just 13, Just 52, Just 26, Just 79, Just 10, Just 27, Just 86, Just 20, Just 72, Just 11, Just 93] = x:xs
    | otherwise = skipUntilOpCode xs
      
