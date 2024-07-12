--runghc hello.hs

module Main where

main :: IO ()
main = putStrLn "Olá, mundo!"

allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == m) && (m == p)
{-
all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal m n o x 
  |allEqual m n o && allEqual n o x = True
  |otherwise = False

maxi :: Int -> Int -> Int
maxi n m 
 | n >= m = n
 | otherwise = m

maxis :: Int -> Int -> Int -> Int
maxis n m o
  | n >= m && n >= o = n
  | m >= n && m >= o = m
  |otherwise = o

addD :: Int -> Int -> Int
addD a b = 2 * (a+b)

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c 
  |allEqual a b c == True = 3
  |a == b || b == c || a==c = 2
  |otherwise = 1

sales :: Int -> Int
sales x = x
totalSales :: Int -> Int
totalSales n
  | n == 0 = sales 0
  | otherwise = totalSales (n-1) + sales n
-}