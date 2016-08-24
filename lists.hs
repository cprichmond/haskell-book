-------Excercises 9.5

eftBool :: Bool -> Bool -> [Bool]
eftBool x y
  | x > y     = []
  | x == y    = [x]
  | otherwise = [x, y]

eftOrdering :: Ordering -> Ordering -> [Ordering]
eftOrdering x y
  | x > y = []
  | x == y = [x]
  | otherwise = x : eftOrdering (succ x) y

eftInt :: Int -> Int -> [Int]
eftInt x y
  | x > y = []
  | x == y = [x]
  | otherwise = x : eftInt (succ x) y

-- eftInt 1 3
--   eftInt 1 : eftInt 2 3
--     eftInt 1 : 2 : eftInt 3 3
--       eftInt 1 : 2 : [3]

eftChar :: Char -> Char -> [Char]
eftChar x y
  | x > y = []
  | x == y = [x]
  | otherwise = x : eftChar (succ x) y

-------Excercises 9.6

--myWords "all i wanna do is have some fun"
myWords :: String -> [String]
myWords "" = []
myWords x
  | dropWhile (/=' ') x == "" = [x] -- base case
  | otherwise = takeWord : myWords (dropWhile (==' ') dropNonWS)
    where
      takeWord = takeWhile (/=' ') x
      dropNonWS = dropWhile (/=' ') x

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

myLines :: String -> [String]
myLines "" = []
myLines x
  | dropWhile (/='\n') x == "" = [x]
  | otherwise = takeLine : myLines (dropWhile (=='\n') dropLine)
    where
      takeLine = takeWhile (/='\n') x
      dropLine = dropWhile (/='\n') x

mySplitter :: Char -> String -> [String]
mySplitter _ "" = []
mySplitter sep x
  | dropWhile (/=sep) x == "" = [x] -- base case
  | otherwise = takeWhileNotSep : mySplitter sep (dropWhile (==sep) dropWhileNotSep)
    where
      takeWhileNotSep = takeWhile (/=sep) x
      dropWhileNotSep = dropWhile (/=sep) x

myWords' :: String -> [String]
myWords' = mySplitter ' '

myLines' :: String -> [String]
myLines' = mySplitter '\n'

--myLines' '\n' sentences
