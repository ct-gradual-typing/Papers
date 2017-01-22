module Fix where

import Prelude hiding (map,    foldr, foldl, zip, zipWith,
                       length, elem,  append)

fix :: (a -> a) -> a
fix f = f (fix f)

--(([a] -> [b]) -> ([a] -> [b])) -> ([a] -> [b])

lcase :: [a] -> b -> (a -> [a] -> b) -> b
lcase [] t1 _ = t1
lcase (x:xs) _ t2 = t2 x xs

map :: (a -> b) -> ([a] -> [b])
map f = fix (\r l->lcase l [] (\x xs->(f x):(r xs)))

append :: ([a] -> [a] -> [a])
append = fix (\r l1 l2->lcase l1 l2
               (\x xs->lcase l2 l1 (\_ _-> x : (r xs l2) ))) 

foldr :: (a -> b -> b) -> b -> ([a] -> b)
foldr op x = fix (\r l -> lcase l x (\y ys -> y `op` (r ys)))

foldl :: (b -> a -> b) -> (b -> [a] -> b)
foldl op = fix (\r x l ->lcase l x (\y ys->r (x `op` y) ys))

zipWith :: (a -> b -> c) -> ([a] -> [b] -> [c])
zipWith f = fix
     (\r l1 l2 -> lcase l1 (lcase l2 [] (\y ys -> []))
        (\x xs -> lcase l2 [] (\y ys -> (f x y) : (r xs ys))))

zip :: [a] -> [b] -> [(a,b)]
zip = zipWith (\x y -> (x,y))

length :: ([a] -> Integer)
length = fix (\r l -> lcase l 0 (\x xs->1 + (r xs)))

elem :: Eq a => a -> ([a] -> Bool)
elem e = fix (\r l -> lcase l False (\x xs->(x == e) && (r xs)))