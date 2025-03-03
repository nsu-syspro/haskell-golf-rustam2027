module Task1 where

-- | Compresses given data using run-length encoding.
--
-- Usage example:
--
-- >>> encode "aaabbccaadaaa"
-- [(3,'a'),(2,'b'),(2,'c'),(2,'a'),(1,'d'),(3,'a')]
-- >>> encode "abc"
-- [(1,'a'),(1,'b'),(1,'c')]
-- >>> encode []
-- []
--
encode :: Eq a => [a] -> [(Int, a)]
encode = foldr func []
  where
    -- func :: Eq a => a -> [(Int, a)] -> [(Int, a)]
    func x ((n, y):accs)| x == y = (n + 1, x) : accs
                        | otherwise = (1, x) : ((n, y):accs)
    func x [] = [(1, x)]

-- | Decompresses given data using run-length decoding.
--
-- Usage example:
--
-- >>> decode [(3,'a'),(2,'b'),(2,'c'),(2,'a'),(1,'d'),(3,'a')]
-- "aaabbccaadaaa"
-- >>> decode [(1,'a'),(1,'b'),(1,'c')]
-- "abc"
-- >>> decode []
-- []
--
decode :: [(Int, a)] -> [a]
decode = concatMap copyN

copyN :: (Int, a) -> [a]
copyN (0, _) = []
copyN (n, x) = x : copyN (n - 1, x)

-- | Rotates given finite list to the left for a given amount N
--
-- If N is negative, then rotates to the right instead.
--
-- Usage example:
--
-- >>> rotate 3 "abcdefgh"
-- "defghabc"
-- >>> rotate (-2) "abcdefgh"
-- "ghabcdef"
-- >>> rotate 0 "abcdefgh"
-- "abcdefgh"
-- >>> rotate 5 "abc"
-- "cab"
-- >>> rotate 5 ""
-- ""
--
rotate :: Int -> [a] -> [a]
rotate 1 (x:xs) = xs ++ [x]
rotate n l | n > 0 = rotate (n-1) $ rotate 1 l
            | n == 0 = l
            | otherwise = rotate (length l + n) l
