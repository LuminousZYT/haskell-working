module Recursive 
( Tree (..)
, List (..)
, (.++)
, singleton
, treeInsert
, treeElem
) where


-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- data List' a = Empty' 
--     | Cons' { listHead :: a
--            , listTail :: List' a
--            } deriving (Show, Read, Eq, Ord)

{- 可以只用特殊字符来定义函数，这样他们就会自动具有中缀的性质 -}          
infixr 5 :-:
data List a = Empty
    | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)




{- 实现二元搜寻树 -}

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)


treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right







{- 定一了一个红绿灯状态 -}
data TrafficLight = Red | Yellow | Green

{- 让TrafficLight类型成为Eq的instance -}
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

{- 让TrafficLight类型成为Show的instance -}
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"





