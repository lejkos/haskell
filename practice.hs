
-- replicate
myReplicate :: Int -> a -> [a]
myReplicate 0 _   = []
myReplicate 1 x   = [x]
myReplicate num x = [x] ++ myReplicate (num-1) x


-- take
myTake :: Int -> [x] -> [x]
myTake 0 _        = []
myTake _ []       = []
myTake num (x:xs) = [x] ++ myTake (num-1) xs


-- repeat
myRepeat :: a -> [a]
myRepeat x = [x] ++ myRepeat x

myRepeat' :: a -> [a]
myRepeat' x = x : myRepeat x


-- zip 
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _           = []
myZip _ []           = []
myZip (x:xs) (y: ys) = [(x, y)] ++ myZip xs ys


-- elem
myElem :: (Eq a) => a -> [a] -> Bool
myElem _ []  = False
myElem needle (x:xs) = if needle == x 
                       then True
                       else needle `myElem` xs


-- quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = let
                       smaller = [a | a <- xs, a <= x]
                       bigger = [b | b <- xs, b > x]
                   in 
                       quicksort smaller ++ [x] ++ quicksort bigger


quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = quicksort smaller ++ [x] ++ quicksort bigger
                    where
                       smaller = [a | a <- xs, a <= x]
                       bigger = [b | b <- xs, b > x]
                       


