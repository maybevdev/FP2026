import System.Win32 (COORD(xPos))
-- I. Mit csinálnak az alábbi függvényhívások, ahol az atlag a számok átlagát meghatározó függvény?

atlag :: (Floating a) => [a] -> a
atlag ls = (sum ls) / fromIntegral (length ls)

-- (atlag . filter (>= 4.5)) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- atlag $ filter (< 4.5) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- (take 4 . reverse . filter odd ) [1..20]
--  take 4 . reverse . filter odd $ [1..20]
--  take 4 ( reverse ( filter odd [1..20]))
--  take 4 $ reverse $ filter odd $ [1..20]


-- - meghatározza egy lista elemszámát, 2 módszerrel (myLength),

myLength [] = 0
myLength(x : xs) = 1 + myLength xs

myLength2 [] res = res
myLength2 (x:xs) res = myLength2 xs (res +1)

myLength3 :: (Foldable t, Num a1, Num (a1 -> a1)) => t a2 -> a1 -> a1
myLength3 ls = foldr (\x db -> (+) 1 db) 0 ls

myLength4 ls = foldl (\db x -> db +1) 0 ls

myProduct [] =1
myProduct ls = x * myProduct xs
    where   
        (x: xs) = ls


myProduct3 ls = foldr (\x -> (*) x) 1 ls
myProduct4 ls res = foldr (\x res -> (*) x res) res ls
myProduct5 ls = foldr1 (\x -> (*) x) ls

myMin [x] = x
myMin (x1:x2:xs) = if x1 < x2 then myMin (x1:xs) else myMin (x2:xs)

myMin2 [x]=x
myMin2 (x1:x2:xs)
    | x1 < x2 = myMin2 (x1: xs)
    | otherwise = myMin2 (x2:xs)


myMin3 ls = foldr1 min ls

myMin4 ls = minimum ls

myMax [x] = x
myMax (x1:x2:xs) = if x1 > x2 then myMax (x1:xs) else myMax (x2:xs)
-- ugyanigy guardokkal vagy foldokkal, mint a minimumnal

listaN ls n = ls !! n

listaN2 ls n 
    | ls == [] = error "ures lista"
    | n < 0 = error "negativ index"
    | length ls <= n = error "tul nagy index"
    | otherwise =  ls !! n

listaFuz ls1 ls2 = ls1 ++ ls2


palindrom  ls = if ls == reverse ls then "palindrom" else "nem palindrom"


