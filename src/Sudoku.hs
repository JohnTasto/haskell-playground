-- From "Thinking Functionally With Haskell" by "Richard Bird"

module Sudoku where

type Matrix a = [Row a]
type Row a    = [a]

type Grid  = Matrix Digit
type Digit = Char

digits :: [Digit]
digits = ['1' .. '9']

blank :: Digit -> Bool
blank = (== '0')

solutions :: Grid -> [Grid]
solutions = search . choices

search :: Matrix [Digit] -> [Grid]
search cm
  | not (safe pm) = []
  | complete pm   = [extract pm]
  | otherwise     = concatMap search (expand1 pm)
  where pm = fully prune cm

choices :: Grid -> Matrix [Digit]
choices = map $ map $ \d -> if blank d then digits else [d]

-- filter valid (expand m) = [extract m]
extract :: Matrix [Digit] -> Grid
extract = map (map head)

-- safe . prune = safe
safe :: Eq a => Matrix [a] -> Bool
safe cm = all possible (rows cm)
       && all possible (cols cm)
       && all possible (boxes cm)

possible :: Eq a => [[a]] -> Bool
possible r = nodups [x | [x] <- r]

nodups :: Eq a => [a] -> Bool
nodups []     = True
nodups (x:xs) = x `notElem` xs && nodups xs

complete :: Matrix [a] -> Bool
complete = all (all single)

single :: [a] -> Bool
single [_] = True
single  _  = False

-- rows . rows = id
rows :: Matrix a -> Matrix a
rows = id

-- cols . cols = id
cols :: Matrix a -> Matrix a
cols = transpose

-- boxes . boxes = id   (on 3^2 x 3^2 matrices)
boxes :: Matrix a -> Matrix a
boxes = map ungroup . ungroup . map transpose . group . map group

transpose :: Matrix a -> Matrix a
transpose = foldr (zipWith (:)) (repeat [])

-- group . ungroup = id
group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs : group (drop 3 xs)

-- ungroup . group = id
ungroup :: [[a]] -> [a]
ungroup = concat


-- filter valid . expand = filter valid . expand . prune
prune :: Eq a => Matrix [a] -> Matrix [a]
prune = pruneBy boxes . pruneBy cols . pruneBy rows
        where pruneBy f = f . map pruneRow . f

-- filter nodups . cartprod = filter nodups . cartprod . pruneRow
pruneRow :: Eq a => Row [a] -> Row [a]
pruneRow row = map (remove fixed) row
               where fixed = [x | [x] <- row]

remove :: Eq a => [a] -> [a] -> [a]
remove _ [x] = [x]
remove rs xs = filter (`notElem` rs) xs

fully :: Eq a => (a -> a) -> a -> a
fully f x = if x == x' then x else fully f x'
            where x' = f x


-- sort . expand = sort . concat . map expand . expand1
-- filter valid . sort . expand = filter valid . sort . concat . map expand . expand1
expand1 :: Matrix [a] -> [Matrix [a]]
expand1 rs = [rsl ++ [rl ++ [c]:rr] ++ rsr | c <- cs]
             where (rsl, r, rsr) = case break (any smallest) rs of
                                     (a, b:c) -> (a, b, c)
                                     (_,  _ ) -> error "Matrix is already complete"
                   (rl, cs, rr)  = case break smallest r of
                                     (a, b:c) -> (a, b, c)
                                     (_,  _ ) -> error "Unreachable"
                   smallest cs'  = length cs' == n
                   n             = minimum (counts rs)

counts :: Matrix [a] -> [Int]
counts = filter (/= 1) . map length . concat



-- -- Original, less efficient way:

-- solutions :: Grid -> [Grid]
-- solutions = filter valid . expand . fully prune . choices

-- -- map rows  . expand = expand . rows   (on 3^2 x 3^2 matrices)
-- -- map cols  . expand = expand . cols   (on 3^2 x 3^2 matrices)
-- -- map boxes . expand = expand . boxes  (on 3^2 x 3^2 matrices)
-- expand :: Matrix [a] -> [Matrix a]
-- expand = cartprod . map cartprod

-- -- map (map f) . cartprod    = cartprod . map (map f)
-- -- filter (all p) . cartprod = cartprod . map (filter p)
-- cartprod :: [[a]] -> [[a]]
-- cartprod []       = [[]]
-- cartprod (xs:xss) = [x:ys | x <- xs, ys <- yss]
--                     where yss = cartprod xss

-- valid :: Eq a => Matrix a -> Bool
-- valid m = all nodups (rows m)
--        && all nodups (cols m)
--        && all nodups (boxes m)



-- -- A better solution for `minimum`?:
-- import Data.List (foldl1')
-- minimum :: Ord a => [a] -> a
-- minimum = foldl1' min
