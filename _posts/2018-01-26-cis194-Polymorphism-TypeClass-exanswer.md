---
layout:     post
title:      "[하스켈 기초][CIS194] 다형성과 타입 클래스 연습문제 풀이"
date:       2018-01-26 22:19:00
summary:    "CIS194 5강 다형성과 타입 클래스 연습문제 풀이입니다."
categories: Haskell CIS194 Polymorphism and Type Class Exercise Solution
---

# 다형성과 타입 클래스 연습문제 풀이

[CIS194 5강 다형성과 타입 클래스](http://enshahar.com/haskell/cis194/polymorphism/and/type/class/2018/01/14/cis194-Polymorphism-TypeClass/) 연습문제 풀이입니다.

## 연습문제 1

그냥 정의대로 하면 된다. 날로먹기. 다만 이제부터는 모듈안에 연습문제 답을 넣기로 하자.

`module 모듈이름(외부에 노출하는 함수이름) where`로 파일을 시작하고, `where` 다음에 필요한 내용을 정의 하면 모듈을 정의할 수 있는 것 같다. 하스켈 모듈에 대해 자세한 것은 나중에 따로 살펴볼 것이다(공부를 아직 안했다).

```haskell
-- Ex1.hs
module Ex1(eval) where
import ExprT

eval :: ExprT -> Integer
eval (Lit v) = v
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
```

## 연습문제 2

`parserExp`의 타입을 보면 `(Integer -> a) -> (a -> a -> a) -> (a -> a -> a) -> String -> Maybe a`이기 때문에 `parserExp Lit Add Mul 문자열`을 한 값의 타입은 `Maybe ExprT`가 된다. 따라서 그 값이 `Nothing`인지 아닌지에 따라 처리하면 된다. 

```haskell
-- Ex2_1.hs
module Ex2_1(evalStr) where
import ExprT
import Ex1
import Parser
evalStr :: String -> Maybe Integer
evalStr s =
    case (parseExp Lit Add Mul s) of
        Nothing -> Nothing
        Just exp -> Just (eval exp)
```

한편 `Maybe`라는 타입에도 리스트의 `map`과 같은 적용이 가능하다면 편할 것이다. `Maybe a` 타입의 값인 어떤 `m`이 있고, 함수 `f :: a->b`가 있을 때 `map f m`을 하면 `m`이 `Nothing`인 경우에는 `Nothing`이, `Just x`인 경우에는 `Just (f m)`이 결과값이 된다면 편할 것이다. 실제로 하스켈에서는 이런 `map`을 제공한다. 다만 여기서는 `map`이라는 이름 대신 `fmap`이라는 이름을 사용해야 한다. 실제로 `fmap`은 `Functor`라는 타입 클래스에 정의된 함수이다. 이를 활용하면 다음과 같이 간편하게 `evalStr`을 정의할 수 있다.

```haskell
-- Ex2.hs
module Ex2(evalStr) where
import ExprT
import Ex1
import Parser
evalStr :: String -> Maybe Integer
evalStr s = fmap eval (parseExp Lit Add Mul s)
```

## 연습문제 3

일단 기본적인 틀은 다음과 같을 것이다.

```haskell
-- Ex3.hs
module Ex3(Expr) where
import ExprT           -- ExprT 정의가 여기 들어있음

class Expr a where
  lit :: ...
  add :: ...
  mul :: ...

instance Expr ExprT where
  lit = ...
  add = ...
  mul = ...
```

여기서 `Expr`의 `lit`가 어떤 타입이어야 할까? `ExprT`의 `Lit`의 타입을 ghci에서 확인해 보면 `Integer`를 받아서 `ExprT`를 만들어내는 데이터 생성자라는 사실을 알 수 있다. 비슷하게 `lit`도 `Integer`를 받아서 `a` 타입을 만들어내면 될것 같다. 인스턴스인 `Expr ExprT`에서는 `Integer`를 받아서 `ExprT`를 반환하면 된다. `Expr` 클래스의 타입 파라미터 `a`가 `ExprT`이므로 쉽게 타입을 추론할 수 있다. 

마찬가지 과정으로 `Add`와 `Mul`로부터 `add`와 `mul`의 타입이 각각 `a -> a -> a`임을 알 수 있다. 이렇게 `Expr` 클래스의 `lit`, `add`, `mul`의 타입만 정리하면 다음과 같아진다.

```haskell
-- Ex3.hs
--module Ex3(Expr) where
module Ex3(Expr(lit,add,mul)) where
import ExprT

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a
```

`ExprT`를 `Expr`의 인스턴스로 정의하는 과정은 간단하다. 데이터 생성자에 적절히 각 값을 넘겨주면 된다. 이때 타입을 확인하면서 진행하면 별다른 문제가 없다.

```haskell
instance Expr ExprT where
  lit v = Lit v
  add e1 e2 = Add e1 e2
  mul e1 e2 = Mul e1 e2
```

## 연습문제 4

맨 처음엔 이게 무슨 소리지 하고 고민할 지도 모르겠다. 하지만 무조건 타입 클래스의 인스턴스를 만들려면 `instance 클래스 대상타입 where`과 같이 시작해야 한다는 사실부터 기억한다면 조금 고민이 줄 것이다. 물론 대상 타입 자체가 타입 인자가 있는 타입이라면 `instance 클래스 (대상타입 타입인자1 ... 타입인자n) where`로 시작하면 된다. 그 후 타입 클래스가 요구하는 메서드에 대한 구현이 들어가면 된다.

타입 클래스가 요구하는 메서드의 타입을 살펴보면 가능한 구현이 어떤 것이 될지 유추할 수 있다.


### `Expr Integer`

`Integer`를 생각해보자. 조금 전 설명한 대로 하면 다음과 같이 일단 뼈대를 작성할 수 있다.

```haskell
-- Ex4.hs
module Ex4 where
import Ex3

instance Expr Integer where 
  lit v = ... 
  add e1 e2 = ...
  mul e1 e2 = ...
```

여기서 `lit v`의 타입은 `Integer->Integer` 타입이 되고 그냥 `v` 값을 그대로 반환하면 쉽게 타입을 맞출 수 있음을 알 수 있다. 마찬가지로 `add e1 e2`는 타입이 `Integer -> Integer -> Integer`이기 때문에 `(+)`로 두 인자를 더해주면 된다는 사실을 쉽게 유추할 수 있다. `mul`도 다르지 않다.

>##### `Ex3.Expr` 내의 `lit` 등의 메서드 외부 노출
>
>`Ex3.hs`에서 `module Ex3(Expr) where`이라고 모듈을 정의하면 `Expr`은 모듈 밖에서 볼 수 있지만 `lit`, `mul`, `add` 메서드를 모듈 밖에서 볼 수 없어서 인스턴스를 정의할 수 없다. 따라서 `module Ex3(Expr(lit,add,mul)) where`와 같이 `Expr`이라는 타입 클래스에서 모듈 외부에 노출시킬 메서드 이름을 함께 정의해야 한다.

이런 과정을 거쳐 코드를 정리하면 다음과 같다.

```haskell
-- Ex4.hs
module Ex4 where
import Ex3

instance Expr Integer where 
  lit v = v 
  add e1 e2 = e1 + e2
  mul e1 e2 = e1 * e2
```

### `Expr Bool`, `Expr MinMax`, `Expr Mod7`

특별히 고민할 것이 없다. 그냥 주어진 로직대로 계산하자.

```haskell
instance Expr Bool where 
  lit 0 = False 
  lit _ = True
  add e1 e2 = e1 || e2
  mul e1 e2 = e1 && e2

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where 
  lit v = MinMax v 
  add (MinMax v1) (MinMax v2) = MinMax (max v1 v2)
  mul (MinMax v1) (MinMax v2) = MinMax (min v1 v2)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where 
  lit v = Mod7 (mod v 7)  
  add (Mod7 v1) (Mod7 v2) = Mod7 (mod (v1 + v2) 7)
  mul (Mod7 v1) (Mod7 v2) = Mod7 (mod (v1 * v2) 7)
```

파서를 적용해 쉽게 같은 식에서 다른 연산 결과를 뽑아낼 수 있다.

```haskell
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
```

이 코드를 `Ex4.hs`에 넣고 실행해 보면 다음과 같다.

```haskell

E:\blog\example\haskell\cis194\05_TypeClass
λ ghci Ex4.hs
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from C:\Users\hyunsok\AppData\Roaming\ghc\ghci.conf
[1 of 4] Compiling Parser           ( Parser.hs, interpreted )
[2 of 4] Compiling ExprT            ( ExprT.hs, interpreted )
[3 of 4] Compiling Ex3              ( Ex3.hs, interpreted )
[4 of 4] Compiling Ex4              ( Ex4.hs, interpreted )
Ok, modules loaded: Ex3, Ex4, ExprT, Parser.
*Ex4> testExp
Just (-7)
*Ex4> testInteger
Just (-7)
*Ex4> testBool
Just True
*Ex4> testMM
Just (MinMax 5)
*Ex4> testSat
Just (Mod7 0)
*Ex4>
```


같은 식을 어떤 타입으로 해석하느냐에 따라 타입 클래스가 제대로 작동해서 우리가 원하는 값이 나옴을 볼 수 있다.

## 연습문제 5

`Expr Bool`등의 클래스 확장을 만들어 봤으므로, 타입 클래스를 적용하는 기본 틀은 익혔을 것이다. 여기서 만들어야 할 `Expr` 타입 클래스 인스턴스는 `Expr Program`이다. 

모듈 정의와 타입 클래스 인스턴스 시작 부분은 다음과 같다.

```haskell
-- Ex5.hs
{-# LANGUAGE TypeSynonymInstances FlexibleInstances #-}
module Ex5(compile) where
import StackVM
import Parser

-- Orphan instance Expr Program 오류 방지를 위해 여기서 Expr 클래스 정의
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr Program where
```

정수 상수는 스택 탑에 정수를 넣으면 된다.

```haskell
    lit v = [StackVM.PushI v]
```

덧셈은 첫번째 인자를 계산하고 두번째 인자를 계산하면 스택 맨 위 2 원소가 첫번째 인자와 두번째 인자가 된다는 성질을 사용해서, 두 인자를 계산하는 프로그램을 서로 이어붙인 다음에, `Add` 연산을 추가해주면 된다. 곱셈도 `Add`가 `Mul`이 된다는 점을 제외하고는 덧셈과 동일하다.

```haskell
    add p1 p2 = p1 ++ p2 ++ [StackVM.Add]
    mul p1 p2 = p1 ++ p2 ++ [StackVM.Mul]
```

이제 앞의 연습문제에서 파서와 `Expr` 인스턴스를 연결했던 것처럼 연결하면 프로그램을 컴파일할 수 있다.

```haskell
compile :: String -> Maybe Program
compile = parseExp lit add mul
```

## 연습문제 6

`HasVars` 자체는 그다지 복잡할 게 없다. 문제에 주어진 내용을 코드로 옮기면 된다.

```haskell
class HasVars a where
  var :: String -> a
```

`VarExprT`는 `HasVars`의 인스턴스인 동시에 `Expr`의 인스턴스인 데이터 타입이며, `(M.Map String Integer -> Maybe Integer)` 타입의 별명로 정의하면 된다.

```haskell
type VarExprT = M.Map String Integer -> Maybe Integer
```

(여기서 `M.Map String Integer`이 `M.Map`과 `String`과 `Integer`라는 세 인자를 의미하는 것이 아니라 `M.Map`이라는 타입의 타입 파라미터로 `String`과 `Integer`를 적용한 것이라는 점을 노파심에 적어둔다.)

더 쉬운 것부터 구현해보자. `VarExprT`를 `HasVars`의 인스턴스로 만들기 위해 어떤 일을 해야 할까? `M.Map` 타입으로 정의된 변수 매핑에서 변수 값을 가져오는 함수를 정의하면 된다. `M.Map`에 대해 정의된 `lookup` 함수가 바로 이런 일을 한다.

```haskell
instance HasVars VarExprT where
  var = M.lookup
```

`Expr`의 인스턴스는 연습문제 3~5에서 연습했던 것과 다르지 않다. 타입만 맞추고, 문맥에 맞게 각 연산을 정의해주면 된다.

`lit a`는 변수 매핑과 상관 없이 무조건 정해준 수를 반환해주면 된다.

```haskell
instance Expr VarExprT
  lit a = \_ -> Just a
```

`mul e1 e2`는 `e1`에 대해 매핑을 적용해 얻은 값과 `e2`에 대해 매핑을 적용해 얻은 값을 조합해주면 된다. 이때 두 값이 `Maybe Integer`이므로 그 둘을 조합할 방법을 생각해봐야 한다. `Data.Maybe`에 보면 `isNothing`이 있다. 이를 활용하면 다음과 같이 작성할 수 있을 것이다. `fromJust`는 `Just v`에서 `Just`를 벗겨내고 `v`만 돌려주는 함수다.

```haskell
-- 맨 앞에 import Data.Maybe를 해야 함
  add e1 e2 = \m ->
    if isNothing (e1 m) || isNothing (e2 m) 
      then Nothing
      else Just(fromJust (e1 m) + fromJust (e2 m))
  mul e1 e2 = \m -> 
    if isNothing (e1 m) || isNothing (e2 m) 
      then Nothing
      else Just(fromJust (e1 m) * fromJust (e2 m))
```

다른 방식으로 생각해 보자. 우리는 `e1 m`과 `e2 m`이라는 두 `Maybe Integer` 값에 대해 `+`라는 이항 연산을 적용하고 싶다. 즉, `(+) :: Integer -> Integer -> Integer`를 `Maybe Integer -> Maybe Integer -> Maybe Integer`로 바꿔주는 함수가 있다면 좋을 것이다.

`hoolge`에서 `liftA2`를 보면 `Applicative f => (a -> b -> c) -> f a -> f b -> f c`으로, `(Integer -> Integer -> Integer) -> Maybe Integer -> Maybe Integer -> Maybe Integer)`라는 우리가 만들고 싶은 함수와 맞아 떨어지는 것 같다.

한번 테스트해보자.

```ghci
E:\blog\example\haskell\cis194\05_TypeClass
λ ghci
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from C:\Users\hyunsok\AppData\Roaming\ghc\ghci.conf
Prelude> import Control.Applicative
Prelude Control.Applicative> liftA2 (+) Nothing (Just 10)
Nothing
Prelude Control.Applicative> liftA2 (+) (Just 10) Nothing
Nothing
Prelude Control.Applicative> liftA2 (+) (Just 10) (Just 20)
Just 30
Prelude Control.Applicative>
```

우리가 원하는대로 작동한다. 이를 사용해 다음과 같이 `add`와 `mul`을 쉽게 구현할 수 있다.

```haskell
-- 맨 앞에 import Control.Applicative를 해야 함
add e1 e2 = \m -> liftA2 (+) (e1 m) (e2 m)
mul e1 e2 = \m -> liftA2 (*) (e1 m) (e2 m)
```

이를 사용해 예제를 실행하면 다음과 같다. 변수 값을 바꾸면 `mul (var "x") (add (var "y") (var "x"))`라는 같은 식에서 다른 결과가 나옴을 확인할 수 있다.

```haskell
E:\blog\example\haskell\cis194\05_TypeClass
λ ghci Ex6.hs
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from C:\Users\hyunsok\AppData\Roaming\ghc\ghci.conf
[1 of 1] Compiling Ex6              ( Ex6.hs, interpreted )
Ok, modules loaded: Ex6.
*Ex6> withVars [("x", 6)] $ add (lit 3) (var "x")
Just 9
*Ex6> withVars [("x", 6)] $ add (lit 3) (var "y")
Nothing
*Ex6> withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
Just 54
*Ex6> withVars [("x", 16), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
Just 304
*Ex6>
```


