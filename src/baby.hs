import           Data.Char
import           Data.List
import qualified Data.Map as Map
import           Data.Function

doubleUs x y = doubleMe x + doubleThree y

doubleMe x = x + x + x

doubleThree x = x * 3

doubleSmallNumber x = if x >100
                        then x
                        else  x*2

doubleSmallNumber' x = (if x >100    {- 使用单引号来区分一个稍经修改但差别不大的函数 -}
                        then x
                        else  x*2 ) + 1

doubleSmallNumber'1 x = if x >100
                        then x
                        else  x*2  + 1

conanO'Brien ="It's a-me, Conan O'Brien!"

lostNumbers' = [4,8,15,16,23,48] 

b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs , odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [ c | c <-st, c `elem` ['A'..'Z']]

getEvenNumber xxs = [[x | x <- xs ,even x ] | xs <- xxs]

{- 组合一个三元组，要求三边长度<=10,且为直角三角形，同时b短于斜边，a边短于b边 -}
triangles = [(a,b,c) | c <- [1..10],b <- [1..10],a <- [1..10],a^2+b^2==c^2,a<b,b<c,a+b+c==24]

addThree :: Int -> Int -> Int -> Int
addThree x y z=x+y+z

factorial ::Integer->Integer
factorial n = product [1..n]

{- 单精度浮点数 -}
circumference ::Float->Float
circumference r=2*pi*r

{- 双精度浮点数 -}
circumference' ::Double->Double
circumference' r=2*pi*r

lucky ::(Integral a)=>a->String
lucky 7 ="LUCKY NUMBER SEVEN!"
lucky x="Sorry, you're out of luck, pal!"

sayMe ::(Integral a)=>a->String
sayMe 1="One!"
sayMe 2="Two!"
sayMe 3="Three!"
sayMe 4="Four!"
sayMe 5="Five!"
sayMe x="Not between 1 and 5"

{- 递回函数 -}
factorials :: (Integral a) => a -> a  
factorials 0 = 1
factorials n = n * factorials (n - 1)

{- Tuple使用模式匹配，二维空间中的向量相加 -}
addVectors ::(Num a)=>(a, a)->(a, a)->(a, a)  
addVectors (x1, y1) (x2, y2)=(x1+x2, y1+y2)  


first ::(a, b, c)->a  
first (x, _, _)=x  

second ::(a, b, c)->b  
second (_, y, _)=y  

third ::(a, b, c)->c
third (_, _, z)=z 

{- 实现自己的head函数 -}
head' ::[a]->a  
head' []=error"Can't call head on an empty list, dummy!"
head' (x:_)=x 

{- 这个函数顾及了空List，单元素List，双元素List以及较长的List，不过(x:y:_)这样的模式就不行了，因为它匹配的List长度不固定 -}
tell ::(Show a)=>[a]->String
tell []="The list is empty"
tell (x:[])="The list has one element: "++show x  
tell (x:y:[])="The list has two elements: "++show x++" and "++show y  
tell (x:y:_)="This list is long. The first two elements are: "++show x++" and "++show y 

{- 用模式匹配和递回重新实现length函数 -}
length'' ::(Num b)=>[a]->b  
length'' []=0
length'' (_:xs)=1+length' xs

{- 实现sum,一个List的和就是头部加上尾部的和的和 -}
sum' ::(Num a)=>[a]->a  
sum' []=0
sum' (x:xs)=x+sum' xs

capital :: String->String
capital "" = "Empty string, whoops!"
capital all@(x:xs)="The first letter of "++all++" is "++[x]

{- 算身体质量检测 -}
{- bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!" -}

{- 去重缩减版 -}
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2 
        --   skinny = 18.5
        --   normal = 25.0
        --   fat = 30.0   {- 其中的名字都是一列垂直排开，如果不这样规范，Haskell就搞不清楚它们在哪个地方了 -}
        {- where绑定也可以使用模式匹配 -}
          (skinny, normal, fat)=(18.5, 25.0, 30.0)

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]    
    where bmi weight height = weight / height ^ 2

calcBmis' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis' xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2]

calcBmis'' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis'' xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

max' :: (Ord a) => a -> a -> a  
max' a b   
    | a > b    = a  
    | otherwise = b 

{- 通过反单引号，我们不仅可以以中缀形式调用函数，也可以在定义函数的时候使用它 -}
myCompare :: (Ord a) => a -> a -> Ordering 
a `myCompare` b  
    | a > b    = GT
    | a == b    = EQ
    | otherwise = LT


initials :: String -> String -> String
initials firstname lastname = [a] ++ ". " ++ [b] ++ "."    
    where (a:_) = firstname            
          (b:_) = lastname

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2 
    in sideArea + 2 * topArea

describeList ::[a]->String
describeList xs="The list is "++case xs of []->"empty."                                               
                                           [x]->"a singleton list."                                               
                                           xs->"a longer list."

describeList' ::[a]->String
describeList' xs="The list is "++what xs      
    where what []="empty."         
          what [x]="a singleton list."         
          what xs="a longer list."

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


{-  高阶函数 -}
zipWith' ::(a->b->c)->[a]->[b]->[c]
zipWith' _ [] _=[]
zipWith' _ _ []=[]
zipWith' f (x:xs) (y:ys)=f x y:zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

map'::(a->b)->[a]->[b]
map' _ []=[]
map' f (x:xs) = f x : map f xs

filter'::(a->Bool)->[a]->[a]
filter' _ []=[]
filter' p (x:xs)
    | p x=x:filter' p xs
    | otherwise=filter' p xs

{- 要找出小于100000的3829的所有倍数 -}
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

{- 与Collatz序列有关，取一个自然数，若为偶数就除以2。若为奇数就乘以3再加1，无论任何以任何数字开始，最终的结果都会归1 -}
chain ::(Integral a)=>a->[a]
chain 1=[1]
chain n
    | even n=n:chain (n`div`2)
    | odd n=n:chain (n*3+1)

-- numLongChains ::Int
-- numLongChains = length (filter isLong (map chain [1..100]))
--     where isLong xs = length xs > 15

{- 还可以用lambda（匿名函数）这么写 -}
{- 匿名函数格式：  （\x -> 函数体）   -}
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs >15) (map chain [1..100]))

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f= \x y -> f y x

-- sum'' :: (Num a) => [a] -> a
-- sum'' xs = foldl (\acc x -> acc + x) 1 xs
{- 1是初始值 -}

{- 柯里化更简单 -}
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 1
{- lambda函数(\acc x -> acc + x )与(+)等价 -}

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map'' ::(a->b)->[a]->[b]
map'' f xs=foldr (\x acc->f x:acc) [] xs

fn = ceiling . negate . tan . cos . max 50

{- 加密 -}
encode ::Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in map chr shifted

{- 解密 -}    
decode ::Int -> String -> String
decode shift msg = encode (negate shift) msg



phoneBook=[("betty","555-2938") ,("bonnie","452-2928") ,("patsy","493-2928") ,("lucille","205-2928") ,("wendy","939-8282") ,("penny","853-2492") ]
{- 该方法在空List中取head时引发一个运行时错误 -}
findKey ::(Eq k) => k -> [(k,v)]->v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

{- 递归函数 -}
{- findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key [] = Nothing findKey' key ((k,v):xs) = 
    if key == k then
        Just v
    else 
        findKey' key xs -}

{- 使用`fold`来替代类似的递归函数 -}
findKey'' ::(Eq k)=> k -> [(k,v)] -> Maybe v
findKey'' key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing


fromList' ::(Ord k)=>[(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty