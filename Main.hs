import Data.List
condnumero :: Float -> Float
condnumero x
  | x == 3 = 100
  | x == 5 = 25
  | otherwise = 0

oRPN :: String -> Float
oRPN = head . foldl foldingFunction [] . words
  where
    foldingFunction (x : y : ys) "-" = (x - y) : ys
    foldingFunction (x : y : ys) "+" = (x + y) : ys
    foldingFunction (x : y : ys) "*" = (x * y) : ys
    foldingFunction (x : y : ys) "/" = (x / y) : ys
    foldingFunction (x : xs) "neg1" = negate x : xs
    foldingFunction (x : xs) "condnumero" = (condnumero x) : xs
    foldingFunction (x : xs) "raiz2" = sqrt x : xs 
    foldingFunction xs "sum" = [sum xs] 
    foldingFunction xs "product" = [product xs]
    foldingFunction xs "promedio" = [sum xs / fromIntegral(length xs)] 
    foldingFunction xs numberString = read numberString : xs

main::IO()
main = return()