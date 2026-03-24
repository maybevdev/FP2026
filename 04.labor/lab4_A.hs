import System.Win32 (LOCALESIGNATURE(lsCsbDefault))
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

-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,
-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjeg

-- decPMax x p = maximum (decP x p)

-- - meghatározza az $a$ és $b$ közötti Fibonacci számokat, $a > 50$.

-- III. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista pozitív elemeinek átlagát,
-- - meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,
-- - tükrözi egy lista elemeit,
-- - két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be,
-- - meghatározza egy lista leggyakrabban előforduló elemét.
