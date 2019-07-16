data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Read, Show, Eq)

data Step a = LStep a (Tree a) | RStep a (Tree a) deriving (Read, Show, Eq)

data Position a = Position (Tree a) (Tree a) [Step a] deriving (Read, Show, Eq)

leaf :: a -> Tree a
leaf a = Node a Empty Empty

root :: Tree a -> Maybe (a, Position a)
root Empty = Nothing
root (Node a l r) = Just (a, Position l r [])

left :: a -> Position a -> Maybe (a, Position a)
left _ (Position Empty _ _) = Nothing
left a (Position (Node b l r) t steps) = Just (b, Position l r (LStep a t : steps))

right :: a -> Position a -> Maybe (a, Position a)
right _ (Position _ Empty _) = Nothing
right a (Position t (Node b l r) steps) = Just (b, Position l r (RStep a t : steps))

up :: a -> Position a -> Maybe (a, Position a)
up _ (Position _ _ []) = Nothing
up a (Position l r (LStep b t : steps)) = Just (b, Position (Node a l r) t steps)
up a (Position l r (RStep b t : steps)) = Just (b, Position t (Node a l r) steps)

goLeft :: Maybe (a, Position a) -> Maybe (a, Position a)
goLeft Nothing = Nothing
goLeft (Just (a, p)) = left a p

goRight :: Maybe (a, Position a) -> Maybe (a, Position a)
goRight Nothing = Nothing
goRight (Just (a, p)) = right a p

goUp :: Maybe (a, Position a) -> Maybe (a, Position a)
goUp Nothing = Nothing
goUp (Just (a, p)) = up a p

-- Example
t = Node 1 (Node 2 (leaf 4) (leaf 5)) (Node 3 (leaf 6) (leaf 7))
p1 = root t
p2 = goLeft p1
p3 = goRight p2
