import Data.List

-- I. Definiáljuk azt a Haskell-listát, amely tartalmazza:

-- - az első n páros szám négyzetét,

parosNegyzet n  = [i ^2 | i <- [0,2 .. n * 2]]

-- - az első $$[1, 2, 2, 3, 3, 3, 4, 4, 4, 4,\ldots]$$,

szamokLs n
    | n == 1 = replicate n n 
    | otherwise = szamokLs (n-1) ++ replicate n n

-- - az első $$[2, 4, 4, 6, 6, 6, 8, 8, 8, 8\ldots]$$,
szamokLs1 n i
    | i /= n = replicate i (i* 2) ++ szamokLs1 n (i + 1)
    | otherwise = replicate i (i *  2)

-- - az első $$[n, n-1, \ldots, 2, 1, 1, 2, \ldots, n-1, n]$$
lsN n = [n, n-1 .. 1] ++ [1 .. n]
lsN2 n = reverse [1 .. n] ++ [1..n]

-- - váltakozva tartalmazzon True és False értékeket,
tf n = [even i | i <- [0 .. n]]

tf3 n  = replicate n [True,False]


-- - váltakozva tartalmazza a $$0,\ 1,\ -1$$ értékeket.

nullEgyMinEgy n = take n ls
    where
        ls = [0,1,-1] ++ ls

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy adott szám osztóinak számát,
osztok n = [i| i <- [1..n], mod n i ==0]
osztokSz n = length $ osztok n

osztokSz1 n = foldl (\res i -> if mod n i == 0 then res+1 else res) 0 [1 .. n]


-- - meghatározza egy adott szám legnagyobb páratlan osztóját,
maxParatlanOsztok2 n = last [i | i <- [1 .. n], mod n 1 ==0, odd i]

-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,
decP x p 
    | x < p = [x]
    | otherwise = decP (div x p) p ++ [mod x p]

decPSzam x p = myLength (decP x p)
    where 
        myLength [] = 0
        myLength (_ : ls) = 1+ myLength ls


-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjeg

decPMax x p = myMaximum (decP x p)
    where
        myMaximum [e1] = e1
        myMaximum (e1 : e2 : ls)
            | e1 >e2 = myMaximum (e1:ls)
            |otherwise = myMaximum(e2:ls)

-- - meghatározza az $a$ és $b$ közötti Fibonacci számokat, $a > 50$.

fibo a b = filter (\x -> x>a && x<b) (fibo2 0 1 0)
    where
        fibo2 a1 b1 res
            | res < b1 = res : fibo2 b1 res (res+b1)
            | otherwise = [res]

fibo2 a1 b1 res 
    | res <= b1 = res: fibo2 b1 res (res+b1)
    | otherwise = [res]

-- III. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista pozitív elemeinek átlagát,

atlag ls = (sum ls) / fromIntegral (length ls)
atlagPozitiv ls = atlag [x | x <- ls, x <- ls, x > 0]


-- - meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,

listaN ls n = [i | (idx, i) <- zip [1..] ls, mod i n==0]

listaN2 ls n i
    | i  >=  length ls = []
    | mod i n ==0 = ls !! i : listaN2 ls n (i+1)
    | otherwise = listaN2 ls n (i+1)


-- - tükrözi egy lista elemeit,
-- - két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be,
-- - meghatározza egy lista leggyakrabban előforduló elemét.
-- elof ls = maxElofElem
--     where 
--         elofSzam = maximum $ map length $ (group . sort) ls
--         ls2 = map (\x -> (head x, length x)) $ (group . sort) ls
--         maxElofElem = filter (\x -> snd x == maxElofSzam) ls2

elof :: Ord a => [a] -> a
elof ls = head $ fst $ head $ filter (\x -> snd x == elofSzam) ls2
    where
        elofSzam = maximum $ map length $ (group . sort) ls
        ls2      = map (\x -> (x, length x)) $ (group . sort) ls