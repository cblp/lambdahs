Заранее импортируем пару полезных функций.
Это должно быть написано сверху, чтобы заработало всё, что написано ниже.
Не обращайте внимания, так надо.

> import Data.Foldable (for_)
> import Data.String (IsString(fromString))


Теперь, собственно, начало.


Лямбда-исчисление
=================

    Ля́мбда-исчисле́ние (λ-исчисление) — формальная система, разработанная
    американским математиком Алонзо Чёрчем для формализации и анализа понятия
    вычислимости.

    (Википудия)


Базовые понятия
===============

Рассмотрим функции

    square(x) = x * x
    power2(x) = x * x

Это разные функции или одинаковые?
Их действие одинаковое и может быть записано таким образом:

    x -> x * x

Можно говорить о равенстве функций

    square  ==  power2  ==  x -> x * x

Такая форма записи функции, не привязанная к имени,
называется "(функциональной) абстракцией".


Строгое описание
================

Язык состоит из таких выражений:

1.  Переменные. Просто какие-то имена.

    Грамматика:

        Expression ::= Variable | ...

    Примеры:

        x
        y
        колбаса

2.  Аппликация, или применение функции.
    В общем случае можно применять любое выражение к любому.

    Грамматика:

        Expression ::= (Expression Expression) | ...

    Примеры:

        (f x)
        (square 4)
        ((f x) (g y))

    В последнем примере результат примения "f" к "x" сам является функцией
    и далее применяется к результату применения "g" к "y".

3.  Лямбда-абстракция — выражение, состоящее из переменной и подвыражения,
    использующего (или нет) эту переменную.

    Грамматика:

        Expression ::= (λ Variable . Expression) | ...

    Примеры:

        (λ x . f x)
        (λ x . (λ y . (add x) y))

Грамматика полностью:

    Expression  ::=   Variable
                  |   (Expression Expression)
                  |   (λ Variable . Expression)

Да! Это полное описания языка лямбда-исчисления.


Определения
===========

Имя переменной — это просто строка:

> type Name = String

Определим тип данных для представления лямбда-выражений:

> data Exp = Var(Name)      -- переменная
>          | App(Exp, Exp)  -- аппликация
>          | Lam(Name, Exp) -- лямбда-абстракция

А это нужно, чтобы легко сравнивать выражения:

>     deriving Eq


Отображение
===========

Опишем преобразование нашей хаскелльной модели в нотацию Чёрча.

> instance Show(Exp) where
>     show(exp) = case exp of
>         Var(name)      -> name
>         App(fun, arg)  -> "(" ++ show(fun) ++ " " ++ show(arg) ++ ")"
>         Lam(var, body) -> "(λ " ++ var ++ " . " ++ show(body) ++ ")"


Синтаксический сахар
====================

Для удобства можно слегка расширить синтаксис без вреда смыслу.

Договоримся, что внешние скобки можно не писать:

    (f x)       ==  f x
    (λ y . e)   ==  λ y . e

Договоримся, что абстракции создаются всегда справа налево:

    λ x . λ y . λ z . e  ==  λ x . (λ y . (λ z . e))

А несколько подряд идущих "λ" можно записывать как одну:

    λ x y z . e  ==  λ x . λ y . λ z . e  ==  λ x . (λ y . (λ z . e))

Порядок сохраняем, он важен!

Реализация:

> lam(vars, body) = case vars of
>     []           -> error("variable list cannot be empty")
>     first : rest -> multiLam(first, rest, body)

> multiLam(first, rest, body) = case rest of
>     []                 -> Lam(first, body)
>     second : remaining -> Lam(first, multiLam(second, remaining, body))

Договоримся, что применяются функции к аргументам всегда слева направо:

    f a b c  ==  ((f a) b) c

Совершенно бесплатно мы получили функции 2 и более аргументов!

Пример:

    определение функции от 2 аргументов:
    add  =  λ x y . {здесь мы как-то вычисляем сумму}

    применение функции к 2 аргументам:
    add 42 24

Реализация:

> app(exps) = case exps of
>     []           -> error("empty list cannot be an expression")
>     first : rest -> multiApp(first, rest)

> multiApp(first, rest) = case rest of
>     []                 -> first
>     second : remaining -> multiApp(App(first, second), remaining)

Чисто техническая плюшечка: разрешим Хаскеллу автоматически преобразовывать
строки (типа `String`) в имя переменной (значение `Var` типа `Exp`).

> instance IsString(Exp) where
>     fromString(name) = Var(name)


Вычисление
==========

А теперь самое интересное. Как, собственно, это исчисление работает?

Выражения можно вычислять путём сокращения.
Правила сокращения зависят от стуктуры выражения:

> eval(exp) = case exp of

Главное правило — аппликация абстракции.
Если функция задана формулой-абстракцией (λ var . body),
то результат применения её к аргументу `arg` —
это подстановка этого аргумента `arg` в тело `body` вместо переменной `var`.

      (λ var . body) arg  -->  body[var//arg]

>     App(Lam(var, body), arg) ->
>         eval(replace(body, var, arg))

И после этого пробуем применить все правила ещё раз.

Операция подстановки реализована ниже.

Если правило не удаётся применить на верхнем уровне,
применяем к отдельным подвыражениям.
Не повторяем правила, если мы не сдвинулись с места.

>     App(fun, arg) -> do
>         let fun1 = eval(fun)
>         let arg1 = eval(arg)
>         if fun1 /= fun || arg1 /= arg then
>             eval(App(fun1, arg1))
>         else
>             exp

Это правило не относится к вычислению,
это правильнее было бы назвать упрощением выражения,
но его удобно реализовать прямо здесь.

Суть этого упрощения в сокращении взаимных абстракции и аппликации:

      λ x . f x  -->  f

Это можно делать, только если "f" не содержит "x".

>     Lam(var1, App(fun, Var(var2))) ->
>         if var1 == var2 && not(contains(fun, var1)) then
>             eval(fun)
>         else
>             exp

Это тоже применение в глубину, но уже для лямбда-абстракции.
Не повторяем правила, если мы не сдвинулись с места.

>     Lam(var, body) -> do
>         let body1 = eval(body)
>         if body1 /= body then
>             eval(Lam(var, body1))
>         else
>             exp

Всё, что не удалось вычислить и упростить, просто оставляем как есть.

>     _ -> exp

Да! Этих правил достаточно, чтобы вычислять что угодно!


Вспомогательные функции
=======================

Подстановка, то есть замена в выражении `exp`
переменной `var` на новое выражение `target`.

> replace(exp, var, target) = case exp of

Встретилась искомая переменная:

>     Var(var2) -> if var2 == var then
>                      target
>                  else
>                      exp

Если лямбда-абстракция вводит переменную с таким же именем,
то это уже совсем другая переменная, мы её не трогаем.

>     Lam(var2, body) -> if var2 == var then
>                            exp
>                        else
>                            Lam(var2, replace(body, var, target))

>     App(fun, arg) -> App(replace(fun, var, target), replace(arg, var, target))

Проверка вхождения переменной в выражение:

> contains(exp, var) = case exp of
>     Var(var1)       -> var1 == var
>     App(exp1, exp2) -> contains(exp1, var) || contains(exp2, var)
>     Lam(var1, body) -> var1 /= var && contains(body, var)


Проверка нашей реализации
=========================

> main = do
>     print("x" :: Exp)
>     print(app["f", "x"])
>     print(Lam("x", app["f", "x"]))

>     let true = lam(["x", "y"], "x")
>     putStr("true = "); print(true)

>     let false = lam(["x", "y"], "y")
>     putStr("false = "); print(false)

>     let test1 = app[true, "ok", "fail"]
>     putStr("test1 = "); print(test1)
>     putStr("      = "); print(eval(test1))

>     let test2 = app[false, "fail", "ok"]
>     putStr("test2 = "); print(test2)
>     putStr("      = "); print(eval(test2))

>     let ifThenElse = lam(["c", "t", "e"], app["c", "t", "e"])
>     putStr("ifThenElse = "); print(ifThenElse)

>     let test3 = app[ifThenElse, true, "ok", "fail"]
>     putStr("test3 = "); print(test3)
>     putStr("      = "); print(eval(test3))

>     let test4 = app[ifThenElse, false, "fail", "ok"]
>     putStr("test4 = "); print(test4)
>     putStr("      = "); print(eval(test4))

>     putStr("ifThenElse = "); print(ifThenElse)
>     putStr("           = "); print(eval(ifThenElse))

>     let or = lam(["a", "b"], app["a", "a", "b"])
>     putStr("or = "); print(or)
>     putStr("   = "); print(eval(or))

>     for_ [false, true] $ \a ->
>         for_ [false, true] $ \b -> do
>             let exp = app[or, a, b]
>             print(a == true, "or", b == true)
>             putStr("\t= "); print(exp)
>             putStr("\t= "); print(eval(exp))
>             putStr("\t= "); print(eval(exp) == true)

>     let and = lam(["a", "b"], app["a", "b", "a"])
>     putStr("and = "); print(and)
>     putStr("    = "); print(eval(and))

>     for_ [false, true] $ \a ->
>         for_ [false, true] $ \b -> do
>             let exp = app[and, a, b]
>             print("and", a == true, b == true)
>             putStr("\t= "); print(exp)
>             putStr("\t= "); print(eval(exp))
>             putStr("\t= "); print(eval(exp) == true)

>     let y = Lam("f",
>                 App(
>                     Lam("x", App("f", App("x", "x"))),
>                     Lam("x", App("f", App("x", "x")))
>                 )
>             )
>     putStr("y = "); print(y)
>     -- putStr("y = "); print(eval(y))
