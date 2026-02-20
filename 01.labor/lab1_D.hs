-- -- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza

-- - két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát,
-- - egy első fokú egyenlet gyökét,
-- - egy szám abszulút értékét,
-- - egy szám előjelét,
-- - két argumentuma közül a maximumot,
-- - két argumentuma közül a minimumot,
-- - egy másodfokú egyenlet gyökeit,
-- - hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.
--   Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.
-- - az n szám faktoriálisát (3 módszer),
-- - az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer). 

osszeg :: (Num a) => a -> a -> a
osszeg a b = a + b

osszeg2 :: Int -> Int -> Int
osszeg2 a b = (+) a b

kulonbseg a b = a-b
kulonbseg2 a b = (-) a b

szorzat a b = a*b
szorzat2 a b = (*) a b

hanyados a b = a / b
hanyados2 a b = a `div` b
hanyados3 a b = div a b

osztmar a b = mod a b
osztmar2 a b = a `mod` b

-- a*x + b 0 -> x = -b/a
elsoF a b = -b / a

abszolut n = if n < 0 then -n else n
abszolut2 n
    | n < 0 = -n
    | otherwise = n

elojel n  = if n < 0 then "negativ" else if n > 0 then "pozitiv" else "nulla"
elojel2 n
    | n < 0 = "negativ"
    | n > 0 = "pozitiv"
    | otherwise  = "nulla"

max1 a b = if a > b then a else b
max2 a b    
    | a > b = a
    | otherwise = b

min1 a b = if a < b then a else b
min2 a b    
    | a < b = a
    | otherwise = b


-- a*(x^2) + b*x +c=0 -> a,b,c
-- delta = b^2 -4*a*c
-- gy1 = (-b + sqrt(delta))/2

-- gy2 = (-b - sqrt(delta))/2
masodF a b c = if delta < 0 then error "komplex gyokok" else (gy1,gy2)
    where 
        delta = b^2 -4*a*c
        gy1 = (-b + sqrt(delta))/2
        gy2 = (-b - sqrt(delta))/2

masodF2 a b c   
    | delta < 0 = error "komplex"
    | otherwise = (gy1,gy2)
    where
        delta = b^2 -4*a*c
        gy1 = (-b + sqrt(delta))/2
        gy2 = (-b - sqrt(delta))/2


elempar :: Eq a => (a, a) -> (a, a) -> Bool
elempar ep1 ep2 = if (a == c && b ==d) || (a == d && b == c) then True else False
    where 
        (a,b) = ep1
        (c,d) = ep2

elempar2 :: Eq a => (a, a) -> (a, a) -> Bool
elempar2 ep1 ep2 = (a == c && b == d) || (a==d && b ==c)
    where
        (a,b)=ep1
        (c,d)=ep2

-- faktorialis
fakt1 0=1
fakt1 n = n * fakt1 (n-1)

fakt2 n
    | n < 0 = -1
    | n == 0 =1
    | otherwise = n *fakt2(n-1)

fakt3 res n
    | n < 0 = -1
    | n == 0 = res
    | otherwise = fakt3(res*n) (n-1)
