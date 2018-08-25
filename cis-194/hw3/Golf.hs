module Golf where

  toIterable :: [a] -> [(a, Integer)]
  toIterable xs = zip xs [1..]

  skips :: [a] -> [[a]]
  skips xs = foldl
      (\x (_, int1) -> x ++
          [
            if int1 == 1
              then xs
              else map fst (filter(\(_, int2) -> mod int2 int1 == 0) (toIterable xs))
          ]
      ) [] (toIterable xs)

  localMaxima :: [Integer] -> [Integer]
  localMaxima (x:y:z:xs)
    | x < y && y > z = y : localMaxima (y:z:xs)
    | otherwise = localMaxima (y:z:xs)
  localMaxima _ = []

  base :: String
  base = "==========\n0123456789\n"

  -- histogram :: [Integer] -> [Integer]
  -- -- TODO

  -- result :: Bool


  main = print ""