import System.Environment (getArgs)
import Data.List


sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ [1..n] \\ sieve
  where sieve = map (\(i, j) -> i + j + 2*i*j)
                . filter (\(i, j) -> i + j + 2*i*j <= n)
                $ cartProd [1..n] [1..n]

-- Return all possible pairs
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [arg] -> 
            case reads arg :: [(Integer, String)] of
                [(n, "")] | n >= 2 -> print $ sieveSundaram n

    