import Data.Foldable
import Data.String

-- Definition ------------------------------------------------------------------

type Name = String

data Exp = Var(Name) | App(Exp, Exp) | Lam(Name, Exp)
    deriving Eq

-- Syntax sugar ----------------------------------------------------------------

instance IsString(Exp) where
    fromString(name) = Var(name)

app(exps) = case exps of
    []            -> error("empty list cannot be an expression")
    first : rest  -> multiApp(first, rest)

multiApp(first, rest) = case rest of
    []                  -> first
    second : remaining  -> multiApp(App(first, second), remaining)

lam(vars, body) = case vars of
    []            -> error("variable list cannot be empty")
    first : rest  -> multiLam(first, rest, body)

multiLam(first, rest, body) = case rest of
    []                  -> Lam(first, body)
    second : remaining  -> Lam(first, multiLam(second, remaining, body))

-- Visualization ---------------------------------------------------------------

instance Show(Exp) where
    show(exp) = case exp of
        Var(name)       -> name
        App(fun, arg)   -> "(" ++ show(fun) ++ " " ++ show(arg) ++ ")"
        Lam(var, body)  -> "(λ " ++ var ++ " . " ++ show(body) ++ ")"

-- Evaluation ------------------------------------------------------------------

eval(exp) = case exp of
    App(Lam(var, body), arg) ->
        eval(replace(body, var, arg))
    App(fun, arg) -> do
        let fun1 = eval(fun)
        let arg1 = eval(arg)
        if fun1 /= fun || arg1 /= arg then
            eval(App(fun1, arg1))
        else
            exp
    Lam(var1, App(fun, Var(var2))) ->
        if var1 == var2 && not(contains(fun, var1)) then eval(fun) else exp
    Lam(var, body) -> do
        let body1 = eval(body)
        if body1 /= body then
            eval(Lam(var, body1))
        else
            exp
    _ -> exp

replace(exp, var, target) = case exp of
    Var(var2)       -> if var2 == var then target else exp
    App(fun, arg)   -> App(replace(fun, var, target), replace(arg, var, target))
    Lam(var2, body) ->
        if var2 == var then exp else Lam(var2, replace(body, var, target))

contains(exp, var) = case exp of
    Var(var1)       -> var1 == var
    App(exp1, exp2) -> contains(exp1, var) || contains(exp2, var)
    Lam(var1, body) -> var1 /= var && contains(body, var)

-- Test ------------------------------------------------------------------------

main = do
    print("x" :: Exp)
    print(app["f", "x"])
    print(Lam("x", app["f", "x"]))

    let true = lam(["x", "y"], "x")
    putStr("true = "); print(true)

    let false = lam(["x", "y"], "y")
    putStr("false = "); print(false)

    let test1 = app[true, "ok", "fail"]
    putStr("test1 = "); print(test1)
    putStr("      = "); print(eval(test1))

    let test2 = app[false, "fail", "ok"]
    putStr("test2 = "); print(test2)
    putStr("      = "); print(eval(test2))

    let ifThenElse = lam(["c", "t", "e"], app["c", "t", "e"])
    putStr("ifThenElse = "); print(ifThenElse)

    let test3 = app[ifThenElse, true, "ok", "fail"]
    putStr("test3 = "); print(test3)
    putStr("      = "); print(eval(test3))

    let test4 = app[ifThenElse, false, "fail", "ok"]
    putStr("test4 = "); print(test4)
    putStr("      = "); print(eval(test4))

    putStr("ifThenElse = "); print(ifThenElse)
    putStr("           = "); print(eval(ifThenElse))

    let or = lam(["a", "b"], app["a", "a", "b"])
    putStr("or = "); print(or)
    putStr("   = "); print(eval(or))

    for_ [false, true] $ \a ->
        for_ [false, true] $ \b -> do
            let exp = app[or, a, b]
            print(a == true, "or", b == true)
            putStr("\t= "); print(exp)
            putStr("\t= "); print(eval(exp))
            putStr("\t= "); print(eval(exp) == true)

    let and = lam(["a", "b"], app["a", "b", "a"])
    putStr("and = "); print(and)
    putStr("    = "); print(eval(and))

    for_ [false, true] $ \a ->
        for_ [false, true] $ \b -> do
            let exp = app[and, a, b]
            print("and", a == true, b == true)
            putStr("\t= "); print(exp)
            putStr("\t= "); print(eval(exp))
            putStr("\t= "); print(eval(exp) == true)

    let y = Lam("f",
                App(
                    Lam("x", App("f", App("x", "x"))),
                    Lam("x", App("f", App("x", "x")))
                )
            )
    putStr("y = "); print(y)
    -- putStr("y = "); print(eval(y))
