{-# OPTIONS_GHC -Wall #-}

module Week4 where 

-- Exercise 1: Wholemeal programming
fun1 :: [Integer] -> Integer
fun1 []      = 1
fun1 (x:xs)
 | even x    = (x - 2) * fun1 xs
 | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x -2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum
    . filter even
    . takeWhile (> 1)
    . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

-- Exercise 2: Folding with trees
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node _ left y right)
  | height left < height right = let newLeft = insert x left
                                 in Node (1 + max (height newLeft) (height right)) newLeft y right
  | otherwise = let newRight = insert x right
                in Node (1 + max (height left) (height newRight)) left y newRight
  where
    height Leaf = -1
    height (Node h _ _ _) = h

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

--Exercise 3: More folds!
xor :: [Bool] -> Bool
xor = foldr xorPattern True
    where 
        xorPattern True False = True
        xorPattern False True = True
        xorPattern _ _ = False

map' :: (a -> b) -> [a] -> [b]
--map' f = foldr (\x y -> f x : y) []
map' = map

-- (Optional)
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' fn acc [] = acc
foldl' fn acc (x:xs) = foldl' fn (fn acc x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' fn acc [] = acc
foldr' fn acc (x:xs) = fn x (foldr' fn acc xs)

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl fn acc [] = acc
--myFoldl fn acc (x:xs) = fn x (foldr fn acc xs)

--Exercise 4: Finding primes
sieveSundaram :: Integer -> [Integer]
sieveSundaram n 
-- | n > 0 = map (\x -> 2 * x + 1) [1.. div (n - 1)  2] 
 | n > 0 = calculatePrimes (combineLists (generateList 1 n) (generateList 1 n))
 | otherwise = []

generateList :: Integer -> Integer -> [Integer]
generateList a b = [a.. div (b - 1) 2]

combineLists :: [Integer] -> [Integer] -> [(Integer, Integer)]
combineLists xs ys = [(x, y) | x <- xs, y <- ys]

calculatePrimes :: [(Integer, Integer)] -> [Integer]
calculatePrimes pairs =  [x * y | (x , y) <- pairs]