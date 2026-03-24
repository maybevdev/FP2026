import Distribution.FieldGrammar (alaList')
-- I. Írjuk meg a beépített 
splitAt' idx ls =(idxElotti,idxUtani)
    where
         idxElotti = take idx ls
         idxUtani = drop idx ls


notElem' e (k: ls)
    | null ls = True
    | e == k = False
    | otherwise = notElem' e ls

elem' :: Eq t => t -> [t] -> Bool
elem' e (k: ls)
    | null ls = False
    | e == k = True
    | otherwise = elem' e ls

concat' lss = foldl1 (++) lss

repeat' n = n : repeat' n
takeRepeat' elemSzam n = take elemSzam $ repeat' n

replicate' n e 
    | n > 1 = e : replicate' (n-1) e
    | otherwise = [e]

cycle' ls = ls ++ cycle' ls
takeCycle n ls = take n $ cycle' ls

iterate' fg e = fg e : iterate' fg (fg e)
takeIterate n fg e = take n $ iterate' fg e 

any' feltetel (x:ls)
    | null ls = False
    | feltetel x == True = True
    | otherwise = any' feltetel ls

all' feltetel (x:ls)
    | null ls = True
    | not (feltetel x)= False
    | otherwise = all' feltetel ls


-- II. Írjunk Haskell-függvényt, amely a foldl vagy a foldr függvényt alkalmazva
-- - implementálja a 
length' ls = foldl (\res x -> res+1) 0 ls
sum' ls = foldl (+) 0 ls

elem1 e ls = foldl (\res x -> if x == e then True else res) False ls

reverse' ls = foldl (\res x -> x : res) [] ls
product' ls = foldl (*) ls

maximum' ls = foldl1 (\res x -> if x > res then x else res) ls

insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs) 
    where
        insert y [] = [y]
        insert y (z:zs) = if y <= z then y:z:zs else z: insert y zs


insertSort' ls = foldr insert [] ls
    where   
        insert x [] = [x]
        insert x (y:ys) = if x <= y then x:y:ys else y: insert x ys

listakfuz lss = foldl (++) [] lss

map' fg ls = foldl (\res x -> res ++ [fg x]) [] ls

filter' feltetel ls = foldl (\res x -> if feltetel x then res ++ [x] else res) [] ls


-- - meghatározza egy lista pozitív elemeinek összegét,
posSum ls = foldl (\res x -> if x > 0 then res + x else res) 0 ls

-- - egy lista páros elemeinek szorzatát,
posProd ls = foldl (\res x -> if even x then res * x else res) 1 ls

-- - n-ig a négyzetszámokat
negyzetSzamokN n = foldl (\res x -> res ++ x^2) [] [1..n]

-- - meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét: $$a_0 + x_0(a_1 + x_0(a_2 + x_0(a_3 + \ldots + x_0(a_{n-1}+ x_0 \cdot a_n))))$$
