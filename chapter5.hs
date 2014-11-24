import Data.Char

firsts ps = [x | (x, _) <- ps]

length' xs = sum [1 | _ <- xs]

factors n = [x | x <- [1..n], n `mod` x == 0]

prime n = factors n == [1, n]

find k t = [v | (k', v) <- t, k == k']

-- find 'b' [('a', 1), ('b', 2), ('c', 3), ('b', 4)]
-- [2, 4]

pairs xs = zip xs (tail xs)
-- pairs [1, 2, 3, 4]
-- [(1, 2), (2, 3), (3, 4)]

sorted xs = and [x <= y | (x, y) <- pairs xs]
-- sorted [1, 2, 3, 4]
-- True
-- sorted [1,3,2,4]
-- False

positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
				where n = length xs - 1
-- positions False [True, False, True, False]
-- [1,3]

lowers xs = length [x| x <- xs, isLower x]
-- lowers "Haskell"
-- 6

count x xs = length [x' | x' <- xs, x' == x]
-- count 's' "Mississippi"
-- 4


-- caesar codec
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8,  4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,  6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0,  0.1]
let2int c = ord c - ord 'a'
int2let n = chr (ord 'a' + n)

shift n c | isLower c = int2let((let2int c + n) `mod` 26)
		| otherwise = c

encode n xs = [shift n x | x <- xs]

percent n m = (fromIntegral n/fromIntegral m) * 100

freqs xs = [percent(count x xs) n | x <- ['a'..'z']]
			where n = lowers xs

chisqr os es = sum [((o - e)^2)/e | (o, e) <- zip os es]

rotate n xs = drop n xs ++ take n xs

crack xs = encode(-factor) xs
			where
				factor = head(positions (minimum chitab) chitab)
				chitab = [chisqr (rotate n table') table | n <- [0..25]]
				table' = freqs xs

-- exercises
-- 1. Using a list comprehension, give an expression that calculates the sum 1^2 + 2^2 + ... + 100^2 of the first one hundred integer squares

sumsquares n = sum [x^2 | x <- [0..n]]

-- 2. In a similar way to the function length, show how the library function replicate :: Int -> a -> [a] that produces a list of identical elements can be dfined using a list comprehension. For example:
-- > replicate 3 True
-- [True, True, True]

replicate' n v = [v | _ <- [1..n]]

-- 3. A triple (x,y,z) of positive integers is a pythagorean if x^2 + y^2 = z^2. Using a list comprehension, define a function pyths :: Int -> [(Int, Int, Int)] that returns the list of all pythagorean triples whose components are at most a given limit. For example:
-- > pyths 10
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]

pyths n = [(x,y,z) | x <- ns, y <- ns, z <- ns, x^2 + y^2 == z^2]
			where
				ns = [1..n]

-- 4. A positive integer is perfect if it equals the sum of its factors, excluding the number itself. Using a list comprehension and the function factors, define a function perfects :: Int -> [Int] that returns the list of all perfect numbers up to a given limit. For example:
-- > perfects 500
-- [6, 28, 496]

factors' n = [x | x <- [1..n-1], n `mod` x == 0]
perfects n = [x | x <- [1..n], sum (factors' x) == x]

-- 5. Show how the single comprehension [(x, y) | x <- [1,2,3], y <- [4,5,6]] with two generators can be re-expressed using two comprehensions with single generators. Hint: make use of the library function concat and nest one comprehension within another
