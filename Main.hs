import Data.String
import GHC.Exts

-- Definition ------------------------------------------------------------------

type VarName = String

data Exp = Var(VarName) | App(Exp, Exp) | Lam(VarName, Exp)
    deriving Eq

-- Syntax sugar ----------------------------------------------------------------

instance IsString(Exp) where
    fromString(varname) = Var(varname)

instance IsList(Exp) where
    type Item(Exp) = Exp
    fromList(exps) = case exps of
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
        Var(varname)    -> varname
        App(fun, arg)   -> "(" ++ show(fun) ++ " " ++ show(arg) ++ ")"
        Lam(var, body)  -> "(λ " ++ var ++ " . " ++ show(body) ++ ")"

-- Evaluation ------------------------------------------------------------------

eval(exp) = case exp of
    App(Lam(var, body), arg) -> eval(replace(body, var, arg))
    App(fun, arg) -> do
        let fun1 = eval(fun)
        let arg1 = eval(arg)
        if fun1 /= fun || arg1 /= arg then
            eval(App(fun1, arg1))
        else
            exp
    _ -> exp

replace(exp, var, target) = case exp of
    Var(var2)       -> if var2 == var then target else exp
    App(fun, arg)   -> App(replace(fun, var, target), replace(arg, var, target))
    Lam(var2, body) ->
        if var2 == var then exp else Lam(var2, replace(body, var, target))

-- Test ------------------------------------------------------------------------

main = do
    print("x" :: Exp)
    print(["f", "x"] :: Exp)
    print(Lam("x", ["f", "x"]))

    let true = lam(["x", "y"], "x")
    putStr("true = "); print(true)

    let false = lam(["x", "y"], "y")
    putStr("false = "); print(false)

    let ifThenElse = lam(["c", "t", "e"], ["c", "t", "e"])
    putStr("ifThenElse = "); print(ifThenElse)

    let test1 = [ifThenElse, true, "ok", "fail"]
    putStr("test1 = "); print(test1)
    putStr("      = "); print(eval(test1))

    let test2 = [ifThenElse, false, "fail", "ok"]
    putStr("test2 = "); print(test2)
    putStr("      = "); print(eval(test2))

    -- -- Expr ifThenElse =
    -- --     Lam("cond", Lam{"then", Lam{"else", App{App{"cond", "then"}, "else"}}});
    -- -- print(ifThenElse);
    --
    -- -- Expr testYes =
    --
    -- -- Expr y = Lam{
    -- --     "f",
    -- --     App{
    -- --         Lam{"x", App{"f", App{"x", "x"}}},
    -- --         Lam{"x", App{"f", App{"x", "x"}}},
    -- --     }
    -- -- };
    -- -- print(y);
