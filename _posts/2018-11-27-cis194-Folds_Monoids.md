---
layout:     post
title:      "[하스켈 기초][CIS194] 폴드와 모노이드"
date:       2018-11-27 00:51:00
summary:    "CIS194 7강 폴드와 모노이드에 대해 배운다"
categories: Haskell CIS194 Folds Monoids
---

# 폴드와 모노이드

읽어볼만한 글들:

- [Learn You a Haskell, Only folds and horses](http://learnyouahaskell.com/higher-order-functions#folds)
- [Learn You a Haskell, Monoids](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids)
- [Fold from the Haskell wiki](http://haskell.org/haskellwiki/Fold)
- [Heinrich Apfelmus, Monoids and Finger Trees](http://apfelmus.nfshost.com/articles/monoid-fingertree.html)
- [Dan Piponi, Haskell Monoids and their Uses](http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html)
- [Data.Monoid documentation](http://haskell.org/ghc/docs/latest/html/libraries/base/Data-Monoid.html)
- [Data.Foldable documentation](http://haskell.org/ghc/docs/latest/html/libraries/base/Data-Foldable.html)

## 다시 폴드

리스트 폴드를 이미 살펴봤지만, 같은 아이디어를 다른 데이터 타입으로 일반화할 수도 있다.

내부 노드에 데이터를 저장하는 이진 트리를 표현하는 다음 데이터 타입이 있다고 하자.

```haskell
data Tree a = Empty
            | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty
```

이 트리의 크기(노드 수)를 계산하는 함수를 작성하자.

```haskell
treeSize :: Tree a -> Integer
treeSize Empty        = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r
```

`Integer`의 트리에서 데이터의 합을 구한다면?

```haskell
treeSum :: Tree Integer -> Integer
treeSum Empty     = 0
treeSum (Node l x r)  = x + treeSum l + treeSum r
```

트리의 깊이를 구하면?

```haskell
treeDepth :: Tree a -> Integer
treeDepth Empty        = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)
```

트리의 원소들을 리스트로 모은다면?

```haskell
flatten :: Tree a -> [a]
flatten Empty        = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r
```

패턴이 보이는가? 위 함수들은 모두 다음과 같은 일을 한다.

1. `Tree`를 입력으로 받는다
2. 입력으로 받은 `Tree`에 대해 패턴 매칭을 수행한다.
3. `Empty` 노드의 경우 간단한 답을 반환한다.
4. `Node` 노드의 경우:
    1. 양 하위 트리에 대해 함수 자신을 재귀적으로 호출한다.
    2. 4.1에서 재귀호출로 얻은 결과들과 `Node`의 데이터 `x`를 모두 조합해 최종 결과를 내놓는다.

좋은 프로그래머라면 반복적으로 나타나는 패턴을 항상 추상화해야 할 것이다. 위 패턴을 일반화해보자. 위 각 예제에서 달라지는 부분을 파라미터로 넘기게 해야 한다.

1. 반환할 값의 타입
2. `Empty`의 경우 어떤 값을 반환할지
3. 재귀 호출의 결과를 어떻게 조합할지

이 함수를 `a` 타입의 데이터가 들어있는 트리에 대해 적용하면 `b`타입의 결과가 나와야 한다.

```haskell
treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty        = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)
```

이제 `treeSize`, `treeSum` 등을 `treeFold`를 사용해 구현할 수 있다.

```haskell
treeSize' :: Tree a -> Integer
treeSize' = treeFold 0 (\l _ r -> 1 + l + r)

treeSum' :: Tree Integer -> Integer
treeSum' = treeFold 0 (\l x r -> l + x + r)

treeDepth' :: Tree a -> Integer
treeDepth' = treeFold 0 (\l _ r -> 1 + max l r)

flatten' :: Tree a -> [a]
flatten' = treeFold [] (\l x r -> l ++ [x] ++ r)
```

이제 트리에 대해 `fold`를 수행하는 함수를 원하는대로 쉽게 정의할 수 있다!

```haskell
treeMax :: (Ord a, Bounded a) => Tree a -> a
treeMax = treeFold minBound (\l x r -> l `max` x `max` r)
```

## 식 폴드하기

`fold`를 적용할만한 다른 곳은 없을까?

숙제에서 살펴본 `ExprT` 타입과 `eval` 함수를 기억해 보자.

```haskell
data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT

eval :: ExprT -> Integer
eval (Lit i)     = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
```

어라? 뭔가 낯익지 않은가? `ExprT`에 대한 `fold`는 어떻게 생겼을까?

```haskell
exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
exprTFold f _ _ (Lit i)     = f i
exprTFold f g h (Add e1 e2) = g (exprTFold f g h e1) (exprTFold f g h e2)
exprTFold f g h (Mul e1 e2) = h (exprTFold f g h e1) (exprTFold f g h e2)

eval2 :: ExprT -> Integer
eval2 = exprTFold id (+) (*)
```

이제 식 안에 들어있는 리터럴의 개수를 세는 등의 다양한 작업을 쉽게 정의할 수 있다.

```haskell
numLiterals :: ExprT -> Int
numLiterals = exprTFold (const 1) (+) (+)
```

### 일반적인 `fold`

여기서 여러분이 기억해야 할 것은 (전부는 아니지만) 상당히 많은 데이터 타입에 대해 `fold`를 정의할 수 있다는 점이다. `T`에 대한 `fold`는 `T`를 만들어내는 여러 생성자에 대한 (고차) 인자를 하나 받는다. 이 인자는 그 생성자들이 저장하는 값을 결과 타입으로 변환하는 방법을 제공한다(다만 이때 `T` 내부에 재귀적으로 `T`가 나타나는 경우, 재귀적인 `T`에 대한 `fold` 결과를 가지고 있다고 가정한다). 우리가 `T`에 대해 작성하려는 함수 중 상당수가 간단한 `fold`로 표현될 수 있다.

## 모노이드

다음은 `Data.Monoid` 모듈에서 찾을 수 있는 한 표준 타입 클래스다.

```haskell
class Monoid m where
    mempty  :: m
    mappend :: m -> m -> m

    mconcat :: [m] -> m
    mconcat = foldr mappend mempty

(<>) :: Monoid m => m -> m -> m
(<>) = mappend
```

`(<>)`는 `mappend`와 같은 뜻이다(GHC 7.4.1부터). `(<>)`를 쓰는 이유는 `mappend`라고 쓰기가 귀찮기 때문이다(`(<>)`를 이항 함수 형태로 쓸 때는 `<>`라고만 쓰면 된다는 점에 유의하라).

`Monoid`의 인스턴스인 타입들은 `mempty`라는 특별한 원소와 `mappend(줄여쓰면 `<>`)`라는 2항 연산자를 가지고 있다. `<>`는 모노이드의 인스턴스인 `m` 타입에 속하는 두 값을 받아서 다시 그 타입(즉, `m`)에 속하는 다른 값을 내놓는다. 여기서 `Monoid` 타입 클래스의 의도상, `mempty`는 `<>` 연산의 항등원(identity)이어야 한다. 그리고 `<>`는 결합 법칙이 성립해야 한다.

이는 다음과 같은 말이다.

1. `mempty <> x == x` (왼쪽 항등원)
2. `x <> mempty == x` (오른쪽 항등원)
3. `(x <> y) <> z == x <> (y <> z)`  (결합법칙)

결합법칙이 성립하면 `<>` 연산을 여러번 적용할 때 다음과 같이 써도 모호성이 없다.

```haskell
a <> b <> c <> d <> e
```

`<>`가 결합법칙이 성립하는 연산이라면, 여기서 어떤 순서로 연산을 수행하든 모든 결과는 동일하다.

또, 리스트에 들어있는 값들을 모두 하나로 조합해주는  `mconcat`이라는 함수도 있다. 이 함수는 기본적으로는 `foldr`을 사용해 구현된다. 하지만 `Monoid`의 인스턴스 중 일부는 이 `mconcat`을 더 효율적으로 구현할 수 있기 때문에 `Monoid` 타입 클래스의 멤버로 `mconcat`을 정의해 두었다.

뒤져보면 `Monoid`는 거의 어디에나 존재한다. 몇가지 모노이드 인스턴스를 작성해 보자(그냥 연습을 위한 것일 뿐이다. 여기서 정의하는 모든 함수는 표준 라이브러리에 이미 들어있다).

리스트는 `++`(연결 연산)에 대해 모노이드이다.

```haskell
instance Monoid [a] where
  mempty  = []
  mappend = (++)
```

위 코드를 보면 알겠지만, 정수와 덧셈을 사용하면 완벽히 좋은 모노이드를 정의할 수 있다(정수 뿐 아니라 유리수, 실수, 복소수 등이 가능하다). 하지만 곱셈의 경우도 완벽한 모노이드다. 이럴 경우 어떻게 해야 할까? 어느 타입이 같은 타입 클래스의 두가지 다른 인스턴스로 작용할 수는 없다. 따라서 새로 두가지 `newtype`을 만들고 각각을 별도의 `Monoid` 인스턴스로 정의한다.

```haskell
newtype Sum a = Sum a
  deriving (Eq, Ord, Num, Show)

getSum :: Sum a -> a
getSum (Sum a) = a

instance Num a => Monoid (Sum a) where
  mempty  = Sum 0
  mappend = (+)

newtype Product a = Product a
  deriving (Eq, Ord, Num, Show)

getProduct :: Product a -> a
getProduct (Product a) = a

instance Num a => Monoid (Product a) where
  mempty  = Product 1
  mappend = (*)
```

여기서 예를 들어 `Integer`의 리스트에 들어있는 원소를 모두 곱한 결과를 `mconcat`으로 구현하려 한다면, 리스트를 먼저 `Product Integer` 타입의 값으로 바꿔야 한다.

```haskell
lst :: [Integer]
lst = [1,5,8,23,423,99]

prod :: Integer
prod = getProduct . mconcat . map Product $ lst
```

(물론 이 예제는 바보같다. 표준 `product` 함수를 사용하면 곱셈을 쉽게 할 수 있다. 하지만 여기서 사용한 방식이 유용할 때가 가끔 있다.)

쌍을 이루는 각 원소들이 모노이드면 그 원소들의 쌍(튜플)도 모노이드다.

```haskell
instance (Monoid a, Monoid b) => Monoid (a,b) where
  mempty = (mempty, mempty)
  (a,b) `mappend` (c,d) = (a `mappend` c, b `mappend` d)
```

>##### 도전과제: 
>
>`Bool`에 대한 `Monoid`의 인스턴스를 만들 수 있는가? 만들 수 있는 인스턴스의 개수가 몇개일까?

>##### 도전과제:
>함수 타입들을 `Monoid`의 인스턴스로 만들 수 있는가?








