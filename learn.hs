
-- 1 of 99 Haskell problems
myLast :: [a] -> a
myLast []     = error "Not possible!"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

myLast' = head . reverse


-- 2 of 99 Haskell problems
myButLast :: [a] -> a
myButLast = last . init

myButLast' :: [a] -> a
myButLast' (x:_:[]) = x
myButLast' (x:xs)   = myButLast' xs


-- 3 of 99 Haskell problems
elementAt :: [a] -> Int -> a
elementAt [] _ = error "List cannot be empty!"
elementAt xs 1 = head xs
elementAt xs pos 
    | pos < 1   = error "Index error"
    | otherwise = elementAt (tail xs) (pos-1)

elementAt' :: [a] -> Int -> a
elementAt' xs pos = xs !! (pos-1)


-- 4 of 99 Haskell problems
myLength :: [a] -> Int
myLength []       = 0
myLength (_:xs)   = 1 + myLength xs


-- 5 of 99 Haskell problems
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]


-- 6 of 99 Haskell problems
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = xs == (reverse xs)




