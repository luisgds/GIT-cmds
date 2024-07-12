import Data.Char (isDigit, isLetter)
--digits

--ghci (name).hs

-- << anwser, cmd, definition

------------------------------------------------- << blocks

------------------------------------------------------- << blocks

square :: Int -> Int
square x = x * x
-- square 3
-- 9
---------------------------------------------------------

allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == m) && (m == p)
--allEqual 1 2 3
--False

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal m n o x 
  |allEqual m n o && allEqual n o x = True
  |otherwise = False
-- 3 3 3 3
-- True
---------------------------------------------------------
maxi :: Int -> Int -> Int
maxi n m 
 | n >= m = n
 | otherwise = m

maxis :: Int -> Int -> Int -> Int
maxis n m o
  | n >= m && n >= o = n
  | m >= n && m >= o = m
  |otherwise = o
-- maxi 2 3 
-- 3
---------------------------------------------------------

addD :: Int -> Int -> Int
addD a b = 2 * (a+b)
-- addD 3 2
-- 10
---------------------------------------------------------

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c 
  |allEqual a b c == True = 3
  |a == b || b == c || a==c = 2
  |otherwise = 1
-- howManyRqual 1 5 5
-- 2
---------------------------------------------------------
sales :: Int -> Int
sales x = x

totalSales :: Int -> Int
totalSales n
  | n == 0 = sales 0
  | otherwise = totalSales (n-1) + sales n
  
averageSales :: Int -> Float
averageSales y 
  | y > 0 = fromIntegral (totalSales y) / fromIntegral y
  |otherwise = 0

--totalSales 4
-- 10
---------------------------------------------------------

addPair :: (Int,Int) -> Int
addPair (x,y) = x+y
-- addPair (1,2)
-- 3
------------------------------------------------------------------------------------------------
shift :: ((Int,Int),Int) -> (Int,(Int,Int))
shift ((x,y),z) = (x,(y,z))
--shift ((1,2),3)
-- (1,(2,3))
-----------------------------------------------

type Name = String 
type Age = Int 
type Phone = Int
type PersonID = (Name, Age, Phone)
name :: PersonID -> Name 
name (n,a,p) = n
idade :: PersonID -> Age 
idade (n,a,p) = a
tele :: PersonID -> Phone
tele (n,a,p) = p
-- let person = ("João", 30, 123456789)
-- name person
----------------------------------------------------------

sumSquares :: Int -> Int -> Int
sumSquares x y = sqX + sqY 
  where sqX = x * x
        sqY = y * y

----------------------------------------------------------
sqroot :: Float -> Float -> Float -> (Float, Float)
sqroot a b c 
  | b*b > 4*a*c = (raizreal1, raizreal2)
  | b*b == 4*a*c = unicaraiz
  | otherwise = (-1, -1)
  where 
    raizreal1 = (-b + sqrt(b*b - 4*a*c)) / (2*a)
    raizreal2 = (-b - sqrt(b*b - 4*a*c)) / (2*a)
    unicaraiz = ( -b / (2*a), -b / (2*a))

---------------------------------------------------------
--lists

sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList as
-- ex:sumList [2,3,4,5]
--14
-----------------------------------

lengthL :: [t] -> Int 
lengthL [] = 0
lengthL (a:as) = 1 + lengthL as
--lengthL [214124,2151,125,6231,124]
--5

concatLists :: [t] -> [t] -> [t]
[] `concatLists` y = y
(x:xs) `concatLists` y = x : (xs `concatLists` y)
--concatLists [123,123,123] [123,123,123]
-- [123,123,123,123,123,123]
-----------------------------------------------------

zipL :: [t] -> [u] -> [(t,u)] 
zipL (a:as) (b:bs) = (a,b): zipL as bs
zipL [] [] = []
-- zipL [1,2,3,4] [5,6,7,8]
-- [(1,5),(2,6),(3,7),(4,8)]
--------------------------------------------------------

myfst :: (t,u) -> t
myfst (x,y) = x

mysnd :: (t,u) -> u
mysnd (x,y) = y

myhead :: [t] -> t
myhead (x:xs) = x

mytail :: [t] -> [t]
mytail (x:xs) = xs
-- myfst (1,2)
-- 1
-- myhead ["abc","d","e","f","g"]
-- abc
--------------------------------------------------------
sumPairs :: [(Int,Int,Int)]->[Int]
sumPairs [] = []
sumPairs ((a, b, c) : xs) = a + b + c : sumPairs xs
--ex: sumPairs [(1,2,3),(3,4,4),(5,5,1),(1,9,1)]
-- [6,11,11,11]

-------------------------------------------------------
digits :: String -> String
digits = filter isDigit
--ex: digits "sfa1849ydbjkas7i"
-- 18497

firstDigit :: String -> Char
firstDigit st = case (digits st) of
          [] -> '\0'
          (a:as) -> a
--firstDigit "ins215ta"
--2

--------------------------------------------------

type Person = String
type Book = String
type Database = [(Person, Book)]

exampleBase :: Database
exampleBase = [("Alice","Postman Pat"), 
               ("Anna","All Alone"), 
               ("Alice","Spot"), 
               ("Rory","Postman Pat")]

books :: Database -> Person -> [Book]
books [] _ = []
books ((person, book):xs) targetPerson
  | person == targetPerson = book : books xs targetPerson
  | otherwise = books xs targetPerson
-- books exampleBase "Alice"
--["Postman Pat","Spot"]

borrowers :: Database -> Book ->[Person]
borrowers [] _ = []
borrowers ((person, book):xs) targetBook
  | book == targetBook = person : borrowers xs targetBook
  | otherwise = borrowers xs targetBook
-- borrowers exampleBase "Postman Pat"
-- ["alice","Rory"]

borrowed :: Database -> Book -> Bool
borrowed [] _ = False
borrowed ((person, book):xs) targetBook
  | book == targetBook = True
  | book /= targetBook = borrowed xs targetBook
-- borrowed exampleBase "Postman Pat"
-- True 

numBorrowed :: Database -> Person -> Int
numBorrowed [] _ = 0
numBorrowed ((person, book):xs) targetPerson
  | person == targetPerson = 1 +  numBorrowed xs targetPerson
  | otherwise = numBorrowed xs targetPerson
-- numBorrowed exampleBase "Alice"
-- 2

makeLoan :: Database -> Person -> Book -> Database
makeLoan [] person book = [(person, book)]  
makeLoan database person book = (person, book) : database
-- makeLoan exampleBase "Pato" "molhado"
--[("Pato","molhado"),("Alice","Postman Pat"),("Anna","All Alone"),("Alice","Spot"),("Rory","Postman Pat")]

returnLoan :: Database -> Person -> Book -> Database
returnLoan [] person book = []
returnLoan ((person, book):xs) targetPerson targetBook
  | targetPerson /= person || targetBook /= book = (person, book) : returnLoan xs targetPerson targetBook 
  | targetPerson == person && targetBook == book = returnLoan xs targetPerson targetBook
-- returnLoan exampleBase "Pato" "molhado"
-- [("Alice","Postman Pat"),("Anna","All Alone"),("Alice","Spot"),("Rory","Postman Pat")]

-------------------------------------------------------------------------

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0
-- isEven 5
-- False

doubleList xs = [2*a|a <- xs]
--doubleList [1,2,3,4,5,6]
--[2,4,6,8,10,12]
doubleIfEven xs = [2*a|a <- xs, isEven a]
--doubleIfEven [1,2,3,4,5,6]
--[4,8,12]

----------------------------------------------

sumPairs2 :: [(Int,Int)] -> [Int]
sumPairs2 lp = [a+b|(a,b) <- lp]

digits2 :: String -> String
digits2 st = [ch | ch <- st, isDigit ch]

member2 :: [Int] -> Int -> Bool
member2 lp n = elem n lp

books2 :: Database -> Person -> [Book]
books2 db targetPerson = [book | (person, book) <- db, person == targetPerson]

borrowers2 :: Database -> Book ->[Person]
borrowers2 db targetBook = [person | (person, book) <- db, book == targetBook]

borrowed2 :: Database -> Book -> Bool
borrowed2 db book = elem book [b | (_, b) <- db]

returnLoan2 :: Database -> Person -> Book -> Database
returnLoan2 db targetperson targetbook = [(person,book)|(person,book) <- db,targetbook /= book || targetperson /= person]

---------------------------------------------------

mymap :: (t -> u) -> [t] -> [u]
mymap f []     = []
mymap f (a:as) = f a : mymap f as

times :: Int -> Int
times x = 2*x

doubleList2 :: [Int] -> [Int]
doubleList2 list = mymap times list


--doubleList2 [1,2,3,45]
-- [2,4,6,90] 

mymap2 f l = [f a | a <- l]

-----------------------------------------------------

total :: (Int->Int)-> Int -> Int
total f 0 = f 0
total f n = total f (n-1) + f n

totalSales2 n = total sales n

--totalSales2 5
-- 15

---------------------------------------------------

maxFun :: (Int -> Int) -> Int -> Int
maxFun f 0 = f 0
maxFun f n = maxi (maxFun f (n-1)) (f n)

maxSales2 :: Int -> Int
maxSales2 a = maxFun totalSales2 a

--maxSales2 5
--15

-------------------------------------------------
zeroInRange :: (Int -> Int) -> Int -> Bool
zeroInRange f 0 = (f 0 == 0)
zeroInRange f n = zeroInRange f (n-1) 
                  || (f n == 0)

isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f a 
  | a == 0 = True
  | a < 0 = False
  | a > 0 && (f a > f (a-1)) = isCrescent f(a-1)
  |otherwise = False

-- isCrescent times 5
-- True

---------------------------------------------------

fold :: (t -> t -> t) -> [t] -> t
fold f [a]    = a
fold f (a:as) = f a (fold f as)
sumList2 l = fold (+) l

-- sumList2 [1,2,3,4]
-- 10

and2 :: [Bool] -> Bool
and2 xs = fold (&&) xs

--and2 [True , True , True, True]
--True
--and2 [False, True , True , True, True]
--False

concat2 :: [[t]] -> [t]
concat2 xs = fold (++) xs

--concat2 [["juca","farinha"],["15"]]
-- ["juca","farinha","15"]

maximum2 :: [Int] -> Int
maximum2 xs = fold maxi xs
-- maximum2 [1,52,12,12,65,2]
-- 65
---------
{-
fold (||) [False, True, True]
fold (++) [“Bom“, “ “, “Dia“]
fold min [6]
fold (*) [1..6]
-}
----------------------------------------------

foldr2 ::(t -> u -> u) -> u -> [t] -> u 
foldr2 f s [] = s
foldr2 f s (a:as) 
            = f a (foldr2 f s as)

concat :: [[t]] -> [t]
concat xs = foldr2 (++) [] xs

and :: [Bool] -> Bool
and bs = foldr2 (&&) True bs

-----------------------------------------------

digits3, letters :: String -> String

filter3 :: (t -> Bool) -> [t] -> [t]
filter3 p []           = []
filter3 p (a:as) | p a = a : filter3 p as
                 | otherwise = filter3 p as

digits3 st  = filter3 isDigit st

-- digits3 "abcdefgh123456ijklmn"
-- "123456"
letters st = filter3 isLetter st

-- letters "abcdefgh123456ijklmn"
-- "abcdefghijklmn"

evens xs = filter3 isEven xs
  where isEven n = (n `mod` 2 == 0)

-- evens [1,2,3,4,5,6,7,8,9,10]
-- [2,4,6,8,10]

filter4 p l = [a | a <- l, p a]

----------------------------------------------

sqrList list = map srq list
  where srq x = x*x

-- sqrList [1,2,4,5,6,7]
-- [1,4,16,25,36,49]

sqrList2 list = fold (*) list

-- sqrList2 [1,2,4,5,6,7]
-- 1680

positive :: (Ord a, Num a) => [a] -> [a]
positive list = filter4 posit list
  where posit x = x > 0

-- positive [-3,-2,-1,0,1,2,3]
-- [1,2,3]

zip1 :: [t] -> [u] -> [(t,u)]
zip1 (a:as) (b:bs) = (a,b): zip1 as bs
zip1   _      _    = []

-- zip1 [1,2,3,4] ["a","b","c"]
-- [(1,"a"),(2,"b"),(3,"c")]

--------------------------------------------------
rev [] = []
rev (a:as) = rev as ++ [a]

--------------------------------------------------

rep 0 ch = []
rep n ch = ch : rep (n-1) ch

-------------------------------------------------

type Person2 = String
type Books2 = String
type Database2 = [(Person2, Books2)]

exampleBase2 :: Database2
exampleBase2 = [("Alice","Postman Pat"), 
               ("Anna","All Alone"), 
               ("Alice","Spot"), 
               ("Rory","Postman Pat")]

books3 :: Database2 -> Person2 -> [Books2]
books3 db per = map snd (filter isPer db)
  where isPer (p,b) = (p == per)
returnLoan3 :: Database2 -> Person2 -> Books2 -> Database2
returnLoan3 db p b = filter notPB db
  where notPB pr = (pr /= (p,b))