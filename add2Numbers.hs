--use :! followed by shell commands to run any shell command in ghci

--pad the list with leading 0's to achieve a desired length
pad::Int->[Int]->[Int]
pad len xs = take (len - length xs) (repeat 0) ++ xs

addArr::[Int]->[Int]->[Int]
addArr xs ys | length ans == 0     = [0]
	     | otherwise           = ans
	where
		ans = dropWhile (==0) (foldr (\s -> \vs -> (s + head vs) `div` 10 : (s + head vs) `mod` 10 : tail vs) [0] (zipWith (+) (pad n xs) (pad n ys)))
			where
				n   = max (length xs) (length ys)
