import Data.Foldable
import Data.List

data Nation = English   | Swede     | Dane  | Norwegian     | German
    deriving (Bounded, Enum, Eq, Show)
data Color  = Red       | Green     | White | Yellow        | Blue
    deriving (Bounded, Enum, Eq, Show)
data Pet    = Dog       | Birds     | Cats  | Horse         | Zebra
    deriving (Bounded, Enum, Eq, Show)
data Drink  = Tea       | Coffee    | Milk  | Beer          | Water
    deriving (Bounded, Enum, Eq, Show)
data Smoke  = PallMall  | Dunhill   | Blend | BlueMaster    | Prince
    deriving (Bounded, Enum, Eq, Show)

answers =
    [ zip5 nation color pet drink smoke

    | color <- values @Color
    , leftOf  color  Green       color White    -- 5

    , nation <- values @Nation
    , first   nation Norwegian                  -- 10
    , same    nation English     color Red      -- 2
    , nextTo  nation Norwegian   color Blue     -- 15

    , drink <- values @Drink
    , middle  drink  Milk                       -- 9
    , same    nation Dane        drink Tea      -- 4
    , same    drink  Coffee      color Green    -- 6

    , pet <- values @Pet
    , same    nation Swede       pet   Dog      -- 3

    , smoke <- values @Smoke
    , same    smoke  PallMall    pet   Birds    -- 7
    , same    color  Yellow      smoke Dunhill  -- 8
    , nextTo  smoke  Blend       pet   Cats     -- 11
    , nextTo  pet    Horse       smoke Dunhill  -- 12
    , same    nation German      smoke Prince   -- 14
    , same    smoke  BlueMaster  drink Beer     -- 13
    , nextTo  drink  Water       smoke Blend    -- 16
    ]

same    xs x  ys y  =  (x, y) `elem` zip xs ys
leftOf  xs x  ys y  =  same  xs x  (tail ys) y
nextTo  xs x  ys y  =  leftOf  xs x  ys y  ||  leftOf  ys y  xs x
middle  xs x        =  xs !! 2 == x
first   xs x        =  head xs == x

values :: (Bounded a, Enum a) => [[a]]
values = permutations [minBound .. maxBound]

main = do
    for_ answers $ \answer -> do
        mapM_ print answer
        print [nation | (nation, _, Zebra, _, _) <- answer]
        putStrLn ""
    putStrLn "No more solutions!"
