module Person.Person
( Person(..)
) where


import qualified Data.Map as Map

{- 定义一个数据类型  人类型 -}
{- data Person = Person String String Int Float String String deriving (Show)

firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname

lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname

age :: Person -> Int
age (Person _ _ age _ _ _) = age

height :: Person -> Float
height (Person _ _ _ height _ _) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ number _) = number

flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor) = flavor -}

{- Record Syntax方法 -}
data Person = Person { firstName :: String    {- ::之前是项的名字，之后是其类型 -}
                    , lastName :: String
                    , age :: Int
                    , height :: Float
                    , phoneNumber :: String
                    , flavor :: String
                    } deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)


{- 类型别名的应用 -}
type PhoneNumber = String
type Name = String           
type PhoneBook = [(Name,PhoneNumber)]
{- phBook是函数，直接给值的 -}
phBook :: PhoneBook
phBook = 
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

{- 取一名字和号码检查它是否存在于电话本 -}
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook








{- 一个例子：有个学校提供了不少壁橱，好给学生们地方放他们的
Gun’N’Rose海报。每个壁橱都有个密码，哪个学生想用个壁橱，就
告诉管理员壁橱的号码，管理员就会告诉他壁橱的密码。但如果这个
壁橱已经让别人用了，管理员就不能告诉他密码了，得换一个壁橱。
我们就用Data.Map的一个Map来表示这些壁橱，把一个号码映射到一
个表示壁橱占用情况及密码的Tuple里。 -}

{- 定义一个壁厨状态类型，有两个值构造子，派生了Show和Eq类型类 -}
data LockerState = Taken | Free deriving (Show, Eq)

{- 定义一个String类型的类型别名Code来表示 -}
type Code = String

{- 用Data.Map的一个Map来表示这些壁橱，把一个号码映射到一个表示壁橱占用情况及密码的Tuple里 -}
{- 按号码找壁橱的Map，Int指壁厨号码 -}
type LockerMap = Map.Map Int (LockerState, Code)

{- 按号码找壁橱的函数 -}
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just(state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

{- Map的例子 -}
lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]