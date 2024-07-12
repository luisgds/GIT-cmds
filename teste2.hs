import Data.Char (isDigit, isLetter)
import Data.Maybe    -- fromJust

-- << anwser
--- << cmd
---- << explication
----- << definition
------------------------------------------------- << blocks

sort1 :: [Int] -> [Int]
sort1 [] = []
sort1 (n:ns) = sort1[ x | x <- ns, x < n] ++ [n] ++ sort1[ y | y <- ns, y >= n]

-------------------------------------------------------------

----(f . g) x = f (g x)

----(.) :: (u -> v) -> (t -> u) -> (t -> v)
----(.) f g x = f (g x)

(>.>) :: (t -> u) -> (u -> v) -> (t -> v)
g >.> f = f . g


twice :: (t -> t) -> (t -> t)
twice f = f . f

--- (twice succ) 12
-- 14

-----------------------------------------------------

iter :: Int -> (t -> t) -> (t -> t)
iter 0 f = id
iter n f = f >.> iter (n-1) f

-----------------------------------------------------------


addNum :: Int -> (Int -> Int)
{-
addNum n = h
  where 
  h m = n + m
-}
addNum n = \m -> n + m

addFive :: Int -> Int
addFive = addNum 5 -- 5 + x

--- addFive 7
-- 12

-------------------------------------------------------------

comp2 :: (t -> u) -> (u -> u -> v) -> (t -> t -> v)
comp2 f g = (\x y -> g (f x) (f y))
-------------------------------------------------------------

{-
g :: u -> t -> v
g = \x y ->f y x

f :: t -> u -> v
f x y = x ++ y
-}

aux0 :: String -> String -> String
aux0 = \x y ->concate y x

concate :: String -> String -> String
concate x y = x ++ y

concatenaInvertida :: String -> String -> String
concatenaInvertida x y = aux0 x y

--- concatenaInvertida "hello" "World!"
-- "World!hello"

-----------------------------------------------------------
{-
multiply :: Int -> Int -> Int
multiply a b = a*b
doubleList :: [Int] -> [Int]
doubleList = map (multiply 2)
(multiply 2) :: Int -> Int
map (multiply 2) :: [Int] -> [Int]

----- finish

whiteSpace = " "
elem :: Char -> [Char] -> Bool
elem ch whiteSpace
\ch -> elem ch whiteSpace
filter (\ch -> not(elem ch whitespace))

--repair
-}

{-
  associatividade
•f a b = (f a) b
•f a b  f (a b)
•t -> u -> v = t -> (u -> v)
•t -> u -> v  (t -> u) -> v
•g :: (Int -> Int) -> Int
g h = h 0 + h 1
-}


curry  :: ((t,u) -> v) -> (t -> u -> v)
curry g a b = g (a,b)
uncurry :: (t -> u -> v) -> ((t,u) -> v)
uncurry f (a,b) = f a b
flip :: (t -> u -> v) -> (u -> t -> v)
flip f b a = f a b

-------------------------------------------------------------------------
-------------------------------------------------------------------------

-- examples: data Bool = True | False ...
data Estacao = Inverno | Verao | Outono  | Primavera
data Temp = Frio | Quente

clima :: Estacao -> Temp
clima Inverno = Frio
clima _       = Quente

----- clima "outo"
-- Quente
type Name = String
type Age = Int
data People = Person Name Age
showPerson :: People -> String
showPerson (Person n a) = n ++ "--" ++ show a

----- let person1 = Person "Jose" 22
--- showPerson person1
-- "Jose--22"

data Shape = Circle Float | Rectangle Float Float
isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False

----- let shape1 = Circle 3.0
----- let shape2 = Rectangle 3.5 1.2
--- isRound shape1
-- True
--- isRound shape2
-- False

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle h w) = h * w

-- area shape1
-- 28.274334
--- area shape2
-- 4.2000003

{- General form
data Nome_do_Tipo
  = Construtor1 t11 ... t1k1
  | Construtor2 t21 ... t2k2
    ....
  | Construtorn tn1 ... Tnkn
  It's recursive and polimorfica
--}

---- recursive 

data Expr = Lit Int       |
            Add Expr Expr |
            Sub Expr Expr

eval :: Expr -> Int
eval (Lit n)     = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

----- let num1 = Lit 5
----- let num2 = Lit 3
----- let sum1 = Add num1 num2
----- let sub1 = Sub (Lit 10) (Lit 16)
--- eval num1
-- 5
--- eval num2
-- 3
--- eval sum1
-- 8 
--- eval sub1
-- -6

----- let eq = Sub (Add (Lit 4) (Lit 5)) (Lit 16)
-- -7

--------------------------------------------------------------------

-- polimorfica

data Pairs t = Pair t t
----- let pairnum  = Pair 6 8
----- let pairtrue = Pair True True 
----- let pairlist = Pair [] [1,3]

data List t = Nil | Cons t (List t)
              --deriving (Eq,Ord,Show)
----- let list1 = Nil 
----- let list2 = Cons 1 Nil
----- let list3 = Cons 1 (Cons 2 (Cons 3 Nil))

data Tree t = NilT | Node t (Tree t) (Tree t)
                  --deriving (Eq,Ord,Show)

----- let raiz       = NilT
-- NilT
----- let folhaleft  = Node 5 (Node 3 NilT NilT) (NilT)
-- Node 5 (Node 3 (NilT) (NilT)) (NilT)tr
----- let treeright  = Node 4 (NilT) (Node 5 (NilT) (Node 6 (NilT) (Node 7 (NilT) (NilT) )))   
-- Node 4 (NilT) (Node 5 (NilT) (Node 6 (NilT) (Node 7 (NilT) (NilT))))

instance Show t => Show (Tree t) where
    show :: Show t => Tree t -> String
    show NilT = "NilT"
    show (Node x left right) = "Node " ++ show x ++ " (" ++ show left ++ ") (" ++ show right ++ ")"

--- raiz
--- folhaleft
--- treeright
{- 
data Tree t = NilT | Node t (Tree t) (Tree t) deriving Show

-- Função para criar uma árvore com 10 elementos de cada lado
createTree :: Int -> t -> Tree t
createTree 0 _ = NilT
createTree n x = Node x (createSubTree 10 x) (createSubTree 10 x)

-- Função auxiliar para criar uma subárvore com n elementos de cada lado
createSubTree :: Int -> t -> Tree t
createSubTree 0 _ = NilT
createSubTree n x = Node x (createSubTree (n-1) x) (createSubTree (n-1) x)

-- Exemplo de uso
main :: IO ()
main = do
  let tree = createTree 10 "Node"
  print tree

>> ghci ...
--- let tree "Hello World!"
-- ...
-}
{-
instance Show t => Show (Tree t) where
    show Nil = "Nil"
    show (Node x left right) = "Node " ++ show x ++ " (" ++ show left ++ ") (" ++ show right ++ ")"
-}

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ " - " ++ showExpr e2 ++ ")"

toList :: List t -> [t]
toList Nil = []
toList (Cons x xs) = x : toList xs

fromList :: [t] -> List t
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

depth :: Tree t -> Int
depth NilT = 0
depth (Node _ left right) = 1 + max (depth left) (depth right)

colapse :: Tree t -> [t]
colapse NilT = []
colapse (Node x left right) = colapse left ++ [x] ++ colapse right

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree _ NilT = NilT
mapTree f (Node x left right) = Node (f x) (mapTree f left) (mapTree f right)

-------------------------------------------------------------------

saldo :: String -> [(String,Float)] -> Maybe Float
saldo _ [] = Nothing
saldo person ((p,s):pss)
              | person == p = Just s
              | otherwise = saldo person pss


-- saldo "Maria" [("Jose", 10), ("Maria",20)] 
-- → Just 20.0 
-- saldo "Pedro" [("Jose", 10), ("Maria",20)] 
-- → Nothing

f :: Float -> Float -> Float
f x y = x / y
---- f 0 1 →  0.0
---- f 1 0 →   ?
f2 :: Float -> Float -> Maybe Float
f2 x y 
    | y == 0 = Nothing
    | otherwise = Just (x / y)
---- f 0 1  →   Just 0.0
---- f 1 0  →   Nothing

--------------------------------------------------------

fat :: Int -> Maybe Int
fat 0 = Just 1
fat n
    | n < 0 = Nothing
    | n > 0 = Just (n * (fromJust (fat (n-1))))

--- fat 5
-- Just 120



