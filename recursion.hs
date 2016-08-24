dividedBy :: (Eq a, Integral a) => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go num denom acc
         | num < denom = (acc, num)
         | otherwise = go (num - denom) denom (acc + 1)



addTo :: Integer -> Integer
addTo n = go n 1
  where go n acc
         | n == 1 = acc
         | otherwise = acc + go (n - 1) (acc + 1)


sumTo :: Integral a => a -> a -> a
sumTo denom n = go denom n 0
  where go denom n acc
          | n == 0 = acc
          | otherwise = go denom (n - 1) (acc + denom)

mc91 :: Integer -> Integer
mc91 n | n > 100 = n - 10
       | otherwise = mc91 $ mc91 (n + 11)
