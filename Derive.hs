data Exp = Add Exp Exp | Mul Exp Exp | Num Integer | Pow Exp Integer | X
    deriving (Eq, Ord)

instance Show Exp where
    show ex = case ex of
        Add a b -> "(" ++ show a ++ " + " ++ show b ++ ")"
        Mul a b -> "(" ++ show a ++ " * " ++ show b ++ ")"
        Num n -> show n
        Pow a n -> "(" ++ show a ++ " ^ " ++ show n ++ ")"
        X -> "x"

instance Num Exp where
    fromInteger n = Num n
    a + b = Add a b
    a * b = Mul a b

derive ex = case ex of
    Add a b -> derive a + derive b
    Mul a b -> derive a * b + a * derive b
    Num _ -> 0
    X -> 1

simplify ex = case ex of
    Add 0 a -> simplify a
    Add a 0 -> simplify a
    Add a b ->
        if a == b then
            simplify (a * 2)
        else
            let (a', b') = reorder (simplify a, simplify b)
            in if a' /= a || b' /= b then simplify (a' + b') else ex
    Mul (Num a) (Num b) -> Num (a * b)
    Mul (Num 1) a -> a
    Mul a (Num 1) -> a
    Mul (Pow a n) (Pow b m) | a == b -> simplify (Pow a (n + m))
    Mul a b ->
        if a == b then
            simplify (Pow a 2)
        else
            let (a', b') = reorder (simplify a, simplify b)
            in if a' /= a || b' /= b then simplify (a' * b') else ex
    Pow a 1 -> simplify a
    Pow (Pow a n) m -> simplify (Pow a (n * m))
    Pow a n -> Pow (simplify a) n
    _ -> ex

reorder (a, b) = if a <= b then (a, b) else (b, a)

main = do
    p 0
    p 42
    p (3 + 3)
    p (3 + X)
    p (X * X)
    p (3 + X^10)

p ex = putStrLn (show (simplify ex) ++ "'\t== " ++ show (simplify (derive ex)))
