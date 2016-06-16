module Queue where

data Queue a = Queue [a] [a]

toList :: Queue a -> [a]
toList (Queue f r) = f ++ (reverse r)

instance Foldable Queue where
    foldMap m = (foldMap m).toList

instance Show a => Show (Queue a) where
    show = show.toList
                
emptyQ :: Queue a
emptyQ = Queue [] []
           
headQ :: Queue a -> a
headQ (Queue (x:xs) _) = x

queue :: [a] -> [a] -> Queue a
queue [] r = Queue (reverse r) []
queue f r = Queue f r

snoc :: Queue a -> a -> Queue a
snoc (Queue f r) x = queue f (x:r)

tailQ :: Queue a -> Queue a
tailQ (Queue (x:f) r) = queue f r
                       
showQueue :: Show a => Queue a -> String
showQueue = show.toList

mapQ :: (a -> b) -> Queue a -> Queue b
mapQ m (Queue f r) = Queue (map m f) (map m r)

fix :: Queue a -> b -> (a -> Queue a -> b -> b) -> b
fix (Queue [] []) base _ = base
fix (Queue [] r) base step = fix (queue [] r) base step
fix (Queue (x:f) r) base step = step x (Queue f r) (fix (Queue f r) base step)
