---
layout:     post
title:      "[하스켈 기초][CIS194] 다형성과 타입 클래스
date:       2018-01-14 21:45:00
summary:    "CIS194 4강 "
categories: Haskell CIS194 High Order Programming Exercise Solution
---

[CIS194 5강 다형성과 타입 클래스](http://www.seas.upenn.edu/%7Ecis194/spring13/lectures/05-type-classes.html) 내용 정리입니다.

# 다형성과 타입 클래스

하스켈은 *파라미터화한 다형성(parametric polymorphism)*을 지원한다. 핵심만 말하자면 모든 입력 타입에 대해 다형적인 함수가 *균일하게(uniformly)* 작동한다는 말이다. 이런 성질로 인해 다형적 함수를 사용하는 사람이나 그런 함수를 개발하는 사람 모두에게 흥미로운 문제가 생긴다.

## 파라미터성

`a -> a -> a`라는 타입을 생각해보자. `a`는 *타입 변수(type variable)*로 어떤 타입이든 될 수 있다. 어떤 함수의 타입이 이런 타입이 될 수 있을까?

```haskell
f :: a -> a -> a
f x y = x && y
```

제대로 작동하지 않음을 알 수 있다. 문법은 문제가 없지만 타입 체크를 통과할 수 없다. 구체적으로 말하면 다음과 같은 오류를 볼 수 있다.

```haskell
2012-02-09.lhs:37:16:
    Couldn't match type `a' with `Bool'
      `a' is a rigid type variable bound by
          the type signature for f :: a -> a -> a at 2012-02-09.lhs:37:3
    In the second argument of `(&&)', namely `y'
    In the expression: x && y
    In an equation for `f': f x y = x && y
```

이 코드가 타입 체크를 통과하지 못하는 이유는 다형적 함수를 *호출하는 쪽*에서 타입을 결정해야 하기 때문이다. 여기서는 다형적 함수를 *구현하는 쪽*에서 특정 타입(`Bool`)을 선택했다. 하지만 이 함수를 호출하는 쪽에서는 `String`이나 `Int` 등의 다른 타입을 선택할 수도 있어야 한다. 다른말로 하자면 `a -> a -> a`라는 타입은 이 함수를 호출하는 쪽에서 정하는 모든 `a` 타입을 다 만족시켜줄 것이라는 *약속*이다.

다룬 구현을 생각해보면 다음과 같다.

```haskell
f a1 a2 = case (typeOf a1) of
            Int  -> a1 + a2
            Bool -> a1 && a2
            _    -> a1
```

여기서 `f`는 일부 타입에 대해서만 약간 다른방식으로 동작한다. 이를 자바는 다음과 같이 구현할 수 있다.

```java
class AdHoc {

    public static Object f(Object a1, Object a2) {
        if (a1 instanceof Integer && a2 instanceof Integer) {
            return (Integer)a1 + (Integer)a2;
        } else if (a1 instanceof Boolean && a2 instanceof Boolean) {
            return (Boolean)a1 && (Boolean)a2;
        } else {
            return a1;
        }
    }

    public static void main (String[] args) {
        System.out.println(f(1,3));
        System.out.println(f(true, false));
        System.out.println(f("hello", "there"));
    }

}

[byorgey@LVN513-9:~/tmp]$ javac Adhoc.java && java AdHoc
4
false
hello
```

하지만 하스켈에서는 이런식으로 코드를 작성할 방법이 없다. 하스켈에는 자바 `instanceof` 연산자와 같은 역할을 하는 기능이 없다. 따라서 어떤 값의 타입을 알아내서 그에 따라 여러 다른 동작을 수행할 수 없다. 하스켈이 그런 기능을 제공하지 않는 이유 중 하나는 컴파일러가 컴파일시 타입 정보를 *지우기(erase)* 때문이다. 실행 시점에는 타입을 물어볼 수 있는 타입 정보가 들어있지 않다! 그리고 (`instanceof`와 같은 연산을 제공하지 않는) 다른 여러 이유도 있다. 그에 대해서는 조금 있다 설명한다.

이런 식의 다형성(`a -> a -> a`)을 *파라미터화한 다형성*이라고 부른다. `f :: a -> a -> a`라는 타입은 `a`라는 타입을 파라미터로 한다. 여기서 *파라미터화*라는 말은 "호출하는 쪽에서 선택한 타입과 관계 없이 균일하게 작동한다*라는 말이다. 자바에서는 *제네릭스(generics)*를 통해 파라미터화한 다형성을 제공한다(하스켈을 설계한 필 와들러(Phillip Wadler)가 자바 제네릭스를 개발할 때 중요한 역할을 했다).

그렇다면 어떤 함수가 `a -> a -> a`라는 타입을 가질 수 있을까? 실제로는 두가지 뿐이다!

```haskell
f1 :: a -> a -> a
f1 x y = x

f2 :: a -> a -> a
f2 x y = y
```

따라서 `a -> a -> a`라는 타입은 상당히 많은 정보를 준다는 사실을 깨달을 수 있다.

한번 파라미터화한 타입을 가지고 놀아보자. 다음 타입들에 대해 이런 타입을 만족시킬 수 있는 함수들이 어떤 동작을 할지 생각해 보라.

- `a -> a`
- `a -> a`
- `a -> b -> a`
- `[a] -> [a]`
- `(b -> c) -> (a -> b) -> (a -> c)`
- `(a -> a) -> a -> a`

### 파라미터화를 바라보는 두가지 관점

(특히 자바처럼 `instanceof`를 제공하는 언어를 사용하는) 다형적인 함수를 *구현하는 사람*이라면 이런 하스켈의 제약이 성가시게 느껴질 것이다. "뭐라고? 거시기를 할 수 없다고?"

하지만 이런 관점에는 동전의 양면 같은 다른 면이 존재한다. 함수를 *사용하는 사람* 입장에서 보면 이는 *제약*이 아니라 *보장*이다. 일반적으로 자신이 어떻게 동작할지 더 확실한 보장을 제공하는 도구가 더 사용하기 편하고 사용시 일어날 일을 추론하기도 편하다. 우리는 이미 `(+)`가 다형적임을 안다(`Int`, `Integer`, `Double` 등에 대해 잘 작동한다). 하지만 실제 덧셈을 하려면 `(+)`가 어떤 타입의 수에 대해 연산을 수행 중인지 알아야만 한다. 두 `Integer`를 더하는 방법과 두 `Double`을 더하는 방법은 전혀 다르다. 그렇다면 어떻게 `(+)`가 제대로 작동할까? 그냥 마법처럼 알아서 잘 작동하는 것일까?

실제로는 그렇지 않다! 이 경우 우리는 실제 하스켈이 타입에 따라 어떤 일을 할지 결정하도록 할 수 있다. 하지만 앞에서 본 것처럼 `isntanceof`와 같은 기능을 사용하지는 않는다. `(+)`의 타입을 보자.

```haskell
Prelude> :t (+) -- 또는 :type (+)
(+) :: Num a => a -> a -> a
```

흠.. 여기서 `Num a =>`는 대체 뭘까? 실제로 `(+)`뿐 아니라 여러 다른 프렐류드 함수들이 타입에 이중화살표(`=>`)를 포함한다. 몇가지 예는 다음과 같다.

```haskell
(==) :: Eq a   => a -> a -> Bool
(<)  :: Ord a  => a -> a -> Bool
show :: Show a => a -> String
```

이런 타입에서 어떤 일이 벌어지는 걸까?

## 타입 클래스

`Num`, `Eq`, `Show`는 모두 *타입 클래스(type class)*다. 따라서 `(==)`, `(<)`, `(+)`와 같은 함수를 "타입 클래스 다형적"이라고 말한다. 직관적으로 설명하자면 타입 클래스는 어떤 정해진 연산이 구현된 *타입의 집합*을 표현한다. 타입 클래스 다형적인 함수는 해당 타입 클래스의 인스턴스(즉, 그런 정해진 연산이 구현된 경우)인 타입에 대해서만 제대로 작동한다. 예를 들어 `Eq` 타입클래스를 보면 다음과 같다.

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

이 코드를 다음과 같이 읽을 수 있다.

> `Eq`는 `a`라는 타입 파라미터를 받는 타입 클래스다. *`Eq`의 인스턴스*가 될 수 있는 타입 `a`에는 `(==)`와 `(/=)`라는 두 함수가 반드시 타입 시그니처에 맞게 정의되어 있어야 한다. 

예를 들어, `Int` 타입이 `Eq` 타입 클래스의 인스턴스가 되려면 `(==) :: Int -> Int -> Bool`와 `(/=) :: Int -> Int -> Bool` 함수가 정의되어 있어야 한다. (물론 표준 프렐류드에 이미 `Int`가 `Eq` 타입 클래스에 속하게 해주는 두 함수가 들어있으므로 여러분이 직접 그 두 함수를 정의할 필요는 없다).

이제 `(==)`의 타입을 다시 보자.

```haskell
(==) :: Eq a => a -> a -> Bool
```

`=>` 왼쪽에 오는 `Eq a`는 *타입 클래스 제약(type class constraint)*이다. `Eq a => a -> a -> Bool`라는 타입은 "`a`가 `Eq` 타입 클래스의 인스턴스라면 `a` 타입의 두 값을 받아서 `Bool`을 내놓는 함수"라는 뜻이다(물론 커링에 대해 배웠으므로 "`a`가 `Eq` 타입 클래스의 인스턴스라면 `a` 타입의 값을 받아서 `a -> Bool` 타입의 함수를 내놓는 함수"라고 말해도 같은 뜻임을 알 것이다). `Eq` 타입 클래스의 인스턴스가 아닌 타입에 대해 `(==)`를 호출하면 타입 오류가 발생한다. 

따라서 일반적인 다형 함수가 타입 인자가 어떤 타입이 되든 그 함수가 잘 작동한다고 약속하는 것이라면, 타입 클래스 다형 함수는 호출하는 쪽에서 어떤 타입 클래스에 속하는 타입을 타입 인자로 속할 때만 제대로 작동한다고 보장하는 약간 더 *제약이 가해진* 약속이라 할 수 있다.

여기서 알아둬야 할 중요한 내용 하나는 타입 클래스 메서드로 정의된 `(==)`가 사용되는 경우 컴파일러가 타입 추론을 통해 `(==)`의 구현 중 어떤 함수를 사용해야 할지 결정한다는 점이다. 즉 타입 클래스는 자바와 같은 언어의 오버로드된(overloaded) 메서드와 비슷한 역할을 한다.

실제 타입 클래스를 어떻게 사용하는지 살펴보기 위해 `Eq`의 인스턴스인 타입을 하나 정의해보자.

```haskell
data Foo = F Int | G Char

instance Eq Foo where
  (F i1) == (F i2) = i1 == i2
  (G c1) == (G c2) = c1 == c2
  _ == _ = False

  foo1 /= foo2 = not (foo1 == foo2)
```

매번 `(==)`와 `(/=)`를 정의하기가 귀찮을 것이다. 실제로는 타입 클래스가 *디폴트 구현*을 제공할 수 있다(이때 디폴트 구현을 타입 클래스에 속한 다른 메서드를 통해 구현할 수 있다). 디폴트 구현이 있더라도 물론 디폴트 구현을 오버라이딩(override)할 수도 있다.

실제 `Eq` 정의는 다음과 같다.

```haskell
class Eq a where
  (==), (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)
```

따라서 `Eq`의 인스턴스를 만들 때는 `(==)`나 `(/=)` 중 더 구현하기 편한 것을 하나만 구현하면 된다. 하나만 구현해도 나머지는 자동으로 우리가 구현한 함수를 통해 구현된다. (물론 구현을 하나도 제공하지 않으면 무한히 상호 재귀호출하게 된다!)

게다가 GHC는 자동으로 `Eq`의 인스턴스를 만들어줄 수도 있다. 몇몇 타입 클래스에 대해 이런 자동 생성 기능을 제공한다. 다음을 보자.

```haskell
data Foo' = F' Int | G' Char
  deriving (Eq, Ord, Show)
```

이 코드는 GHC에게 `Foo`에 대해 `Eq`, `Ord`, `Show` 타입 클래스에 속하는 구현을 자동생성하라고 지시한다.

### 타입 클래스와 자바 인터페이스

타입 클래스는 자바 인터페이스와 비슷하다. 둘 다 정해진 연산들을 제공하는 타입/클래스를 정의한다. 하지만 몇가지 측면에서 타입 클래스가 자바 인터페이스보다 더 일반적이다.

1. 자바 클래스를 정의할 때는 자신이 어떤 인터페이스를 구현하는지 반드시 선언해야 한다. 반면 타입 클래스의 인스턴스를 정의하는 부분과 그 타입을 정의하는 부분은 서로 분리된다(앞의 `Foo`와 `instance Eq Foo`). 심지어 어떤 타입 정의와 그 타입을 타입 클래스의 인스턴스로 정의하는 부분이 서로 다른 모듈에 들어갈 수도 있다.
2. 타임 클래스 메서드에 지정할 수 있는 타입은 자바 인터페이스의 메서드에 대해 지정할 수 있는 타입보다 더 일반적이고 유연한다. 특하 *타입 파라미터가 여럿 있는 타입 클래스*를 생각해보면 그 차이가 더 커진다. 

> 2번의 예로 다음과 같은 타입 클래스가 있다고 하자.
>
>```haskell
>class Blerg a b where
>  blerg :: a -> b -> Bool
>```
>
>`blerg`를 사용하는 것은 *이중 디스패치(double dispatch)*를 하는 것이다. 컴파일러는 `a`와 `b` 타입을 모두 고려해 어떤 `blerg` 구현을 사용할지 결정한다. 자바에서는 더블 디스패치를 구현하기가 간단하지 않다.
>
>다음과 같이 하스켈 타입 클래스는 이항(또는 3항, 4항...) 메서드를 쉽게 처리할 수 있다.
>
>```haskell
>class Num a where
>  (+) :: a -> a -> a
>  ...
>```
>
>자바에서 이항 연산자를 깔끔하게 구현할 방법은 없다. 자바에서는 두 인자 중 하나가 `(+)` 메서드를 호출할 때 "수신객체"가 되어야 하기 때문에 좀 더 "우대받는" 객체가 되어야 한다. 이런 비대칭성은 좀 이상하다. 더 나아가 자바의 하위타입 관계(subtyping)로 인해 어떤 함수의 두 인자를 특장 인터페이스 타입으로 지정한다고 해도 그 두 인자가 동일한 타입이 된다는 보장이 *없다*. 그로 인해 이런 2항(또는 피연산자가 2개보다 많은) 연산자를 구현하려면 복잡해진다(보통은 실행 시점에 타입을 검사해야 한다).

### 표준 타입 클래스

꼭 알아둬야 하는 표준 타입 클래스는 다음과 같다.

- `Ord` : 이 타입에 속하는 원소간에는 순서가 *(완전히)[https://ko.wikipedia.org/wiki/전순서_집합] 집합(total order)* 정의된다. 즉 이 타입에 속하는 모든 두 원소의 크기를 비교할 수 있다. `Ord` 타입은 `(<)`, `(>)`를 제공하고, 그와 더불어 `compare`를 제공한다.
- `Num` : "수" 타입이다. 덧셈, 뺄셈, 곱셈 등의 연산을 지원한다. 한가지 중요한 점은 정수 리터럴이 실제로는 다음과 같이 타입 클래스 다형적이라는 점이다. 따라서 `5`와 같은 리터럴은 `Int`나 `Integer`, `Double` 등 `Num` 타입 클래스의 인스턴스인 어떤 타입의 값으로든 쓰일 수 있다`Rational`, `Complex`, `Double` 등의 타입은 물론이고, 심지어 여러분이 직접 정의한 `Num` 인스턴스에 대해서도 사용 가능하다.

```haskell
Prelude> :t 5
5 :: Num a => a
```

- `Show`: 이 타입 클래스 인스턴스에 속하는 값을 `String`으로 변환하는 `show` 메서드를 정의한다.
- `Read`: `Show`와 듀얼(dual) 관계에 있다. 즉, `String`을 값으로 변환한다.
- `Integral`: `Int`나 `Integer`와 같이 정수를 표현하는 타입을 나타낸다.

### 타입 클래스 예제

직접 타입 클래스를 만들어보자. 다음과 같은 클래스를 생각해보자.

```haskell
class Listable a where
  toList :: a -> [Int]
```

이 `Listable`을 `Int`의 리스트로 변환할 수 있는 대상들로 이뤄진 클래스라고 생각할 수 있다. 우선 `Int` 타입은 원소가 1개뿐인 `Int` 리스트로 변환할 수 있다. `Bool`은 `True`를 `1`, `False`를 `0`으로 해석해서 `Int` 리스트로 변환할 수 있다.

```haskell
instance Listable Int where
  -- toList :: Int -> [Int]
  toList x = [x]

instance Listable Bool where
  -- toList :: Bool -> [Int]
  toList True  = [1]
  toList False = [0]
```

`[Int]`를 `[Int]`로 바꿀 때는 특별한 처리가 필요 없다.

```haskell
instance Listable [Int] where
  toList = id
```

마지막으로 이진 트리 타입이 있다면 트리를 중위 순회 방식으로 펼쳐서 리스트로 만들 수 있다.

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
  toList Empty        = []
  toList (Node x l r) = toList l ++ [x] ++ toList r
```

다른 함수를 `toList`를 가지고 정의한다면 그 함수에 `Listable` 클래스 제약이 따라 붙는다. 예를 들면 다음과 같다.

```haskell
-- sumL을 계산하기 위해 먼저 대상을 [Int]로 변환하고 sum을 한다
sumL x = sum (toList x)
```

`ghci`가 보여주는 `sumL`의 타입은 다음과 같다.

```haskell
sumL :: Listable a => a -> Int
```

이 타입은 말이 된다. `sumL`은 `toList`를 사용하기 때문에 정수 리스트로 만들 수 있는(즉 `Listable`의 인스턴스인) 타입에 대해서만 작동할 수 있다. 다음은 어떨까?

```haskell
foo x y = sum (toList x) == sum (toList y) || x < y
```

`ghci`는 `foo`의 타입을 다음과 같이 추론한다.

```haskell
foo :: (Listable a, Ord a) => a -> a -> Bool
```

즉, `foo`는 `Listable`하면서 `Ord`인 타입에 대해 작동한다. `foo`가 `toList`와 비교연산을 사용하기 때문에 이 타입이 타당하다.

마지막으로 좀 더 복잡한 예제를 보자.

```haskell
instance (Listable a, Listable b) => Listable (a,b) where
  toList (x,y) = toList x ++ toList y
```

이 예제에서는 함수가 아니라 타입 클래스의 인스턴스에 대해 타입 클래스 제약을 걸었다. 이 구현은 `(a, b)`라는 순서쌍 타입이 `Listable`의 인스턴스이려면 `a`와 `b`가 각각 `Listable`의 인스턴스여야 한다는 사실을 명시한다. 그리고 이 인스턴스의 `toList` 메서드를 순서쌍의 두 원소 타입인 `a`와 `b`에 있는 `toList`를 사용해 구현한다. 이때 이 정의가 재귀적이지 *않다는* 사실에 유의하라! 여기서 우리가 정의하는 `toList`는 자신(`Listable (a,b)`)의 `toList`가 아니라 다른 타입(`Listable a`와 `Listable b`)의 `toList`를 호출한다.

## 연습문제 - 계산기 DSL

하스켈의 *타입 클래스*는 *임의 다형성(ad-hoc polymorphism)*을 제공한다. 즉 하스켈은 입력 타입에 따라 어떤 함수를 호출할지 결정해 줄 수 있다. 이번 연습문제에서는 타입 클래스를 사용해 *영역 특화 언어(DSL, Domain Specific Language)*를 정의하는 방법을 살펴본다.

### 식

대박 상품인 계산기의 두뇌를 개발해달라는 의뢰를 받았다. 그 계산기는 다른 계산기와 전혀 같지 않다! 공략 대상 고객을 분석한 결과 고객이 원하는 것은 덧셈과 곱셈 뿐이고 다른 기능은 모두 사용자 인터페이스를 망친다는 사실을 알게됐다.

수석 개발자가 수식을 표현하기 위해 이미 다음과 같은 데이터 타입을 정의했다.

```haskell
data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)
```

이 타입을 사용해 `(2 + 3) * 4`라는 식을 표현하면 다음처럼 된다.

```haskell
Mul (Add (Lit 2) (Lit 3)) (Lit 4)
```

이 `ExprT` 구현은 `ExprT.hs`에 들어있다. 따라서 `ExprT`를 사용하는 파일의 맨 위에 `import ExprT`를 추가하면 된다. 

### 연습문제 1

`ExprT`를 사용해 계산하는 계산기를 만들라. 타입 시그니처는 다음과 같다.

```haskell
eval :: ExprT -> Integer
```

예를 들어 `eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20`이다.

### 연습문제 2

UI팀에서 사용자 인터페이스로 계산기 언어의 [파서](/files/cis194/Parser.hs)를 만들었다. `Parser.hs`에 그 모듈이 들어있고, `parseExp`라는 함수가 산술 식을 파싱해준다. `parseExp`에게 `ExprT`의 생성자들을 인자로 넘기고 문자열을 넘기면 `ExprT`로 변환해준다.

```haskell
*Calc> parseExp Lit Add Mul "(2+3)*4"
Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
*Calc> parseExp Lit Add Mul "2+3*4"
Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))
*Calc> parseExp Lit Add Mul "2+3*"
Nothing
```

이 `parseExp`를 사용해 수식을 계산하는 다음 함수를 작성하라.

```haskell
evalStr :: String -> Maybe Integer
```

### 연습문제 3

다행히 고객 반응이 좋다. 그렇지만 계산기가 계산을 어떻게 처리할지에 대해 사용자 사이에 이견이 좀 있다. `ExprT`를 변형해 여러 다른 고객에 맞춰 서비스를 제공하기 힘들기 때문에, 이제 개발부서(당신!)가 `ExprT`를 더 유연하게 만들 필요가 생겼다. 그래서 당신은 `ExprT`의 여러 특성을 타입 클래스로 추상화하기로 결정했다.

`ExprT` 구축에 필요한 `lit`, `add`, `mul`이라는 세가지 메서드를 제공하는 `Expr`이라는 타입 클래스를 정의하라. `ExprT` 타입을 `Expr`의 인스턴스로 정의하라. 이때 다음과 같이 작동해야 한다.

```haskell
mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
== Mul (Add (Lit 2) (Lit 3)) (Lit 4)
```

`lit`, `add`, `mul`의 타입을 어떻게 지정할지 조심스럽게 고민해봐야 한다. `ExprT` 생성자의 타입을 살펴보면 도움이 될 수도 있다. `ghci` 프럼프트에서 `:t Lit`와 같은 명령을 사용해 타입을 확인하라.

>##### 부연설명
>
>앞의 식의 타입을 살펴보라.
>
>```haskell
>*Calc> :t mul (add (lit 2) (lit 3)) (lit 4)
>Expr a => a
>```
>
>이 타입이 어떤 뜻일까? `mul (add (lit 2) (lit 3)) (lit 4)`라는 식은 *Expr* 타입 클래스의 인스턴스인 *어떤 타입*이든 될 수 있다. 따라서 `mul (add (lit 2) (lit 3)) (lit 4)`라고만 쓰면 모호하다. GHC가 `lit`, `add`, `mul`에 사용할 구체적인 타입을 선택할 수 없다.
>
>이런 모호성을 제거하기 위해 위 예제처럼 타입을 명시할 수 있다. 또 다른 방법으로는 이 식을 구체적으로 타입을 추론할 수 있는 맥락 안에서 사용하는 것이 있다. 예를 들어 다음과 같이 `reify`를 정의할 수 있다.
>
>```haskell
>reify :: ExprT -> ExprT
>reify = id
>```
>
>타입에 대해 훈련을 받지 않은 사람이 본다면 이 `reify`가 하는 일이 전혀 없어 보일 것이다! 하지만 이 `reify` 함수의 목적은 자신의 인자로 들어오는 값에 대해 구체적인 타입(`ExprT`)을 부여하는 것이다. 이제 `reify $ mul (add (lit 2) (lit 3)) (lit 4)`와 같이 쓰면 타입을 따로 명시하지 않아도 된다.

>#### `$`
>
>함수 호출시 괄호가 필수적인 경우가 있다. 예를 들어 `reify mul (add (lit 2) (lit 3)) (lit 4)`라고는 호출할 수 없다. 함수 호출의 결합 법칙이 왼쪽에서 오른쪽이기 때문에 `reify mul (add (lit 2) (lit 3)) (lit 4)`는 `(reify mul) (add (lit 2) (lit 3)) (lit 4)`처럼 해석된다. 이럴 때 사용하는 연산자가 `($)`이다. `($)`의 타입은 `(a -> b) -> a -> b`며 첫번째 인자로 받은 함수에 두번째 인자로 받은 값을 적용해준다. 즉, `f $ x`는 `f x`와 같다. 따라서 `($)` 자체는 불필요하다. 하지만 `($)`의 우선순위가 아주 낮고 오른쪽 결합 법칙을 사용하도록 정의됐기 때문에 `f (g (h x))`를 `f $ g $ h x`라고 쓸 수 있다. 마찬가지로 `map ($ 0) xs`나 `zipWith ($) fs xs`와 같이 고차 함수에서도 유용하게 쓸 수 있다. 이 두 식은 각각 `xs`에 들어있는 함수에 대해 `0`을 적용하거나, `fs`에 들어있는 함수에 `xs`의 원소를 적용하는 역할을 한다. 이때 각각의 `$` 활용을 람다로 쓰면 `\f->f 0`과 `\f x->f x`가 된다.)

### 연습문제 4

마케팅 부서에서 고객들에게 커스텀 계산기를 제공하겠다고 약속을 해 버렸다. 예제 3에서 말했듯이 1차 버전이 나간 후, 고객들은 UI에는 만족했지만 식의 *의미*에 대해 서로 다른 생각을 가지고 있음이 밝혀졌다. `ExprT`에 대한 코드를 작성하면서 정수에 대한 덧셈과 곱셈만으로 시장에서 통하리라고 생각했던 것이 기억나나? 실제로는 그렇지 않다. 몇몇 큰손들은 자신이 원하는대로 계산기가 작동하기를 바란다.

`Expr` 타입 클래스의 핵심은 일단 연산을 한번 정의하고 나면 식이 타입에 따라 다양한 방식으로 해석될 수 있다는 데 있다.

다음 타입에 대한 `Expr`의 인스턴스를 만들라.

- `Integer` - 맨 처음에 만든 `ExprT` 계산기와 마찬가지(정수 덧셈, 곱셈) 기능을 제공함
- `Bool` - `0`은 `False`로, `0`이 아닌 모든 값은 `True`로 해석하라. 따라서 "덧셈"은 논리 합(or), "곱셈"은 논리곱(and)이 된다.
- `MinMax` - "덧셈"은 `max` 함수를 "곱셈"은 `min` 함수를 통해 계산된다.
- `Mod7` - 값이 항상 `0`부터 `6` 사이에 있어야 하며 덧셈과 곱셈의 결과를 `7`로 나눈 값(modulo 7)으로 계산한다. 예를 들어 `5+3=1`이다.

`MinMax`와 `Mod7`은 내부적으로 `Integer`에 대해 작동한다. 하지만 다른 타입 인스턴스를 제공하기 위해 `Integer`를 `newtype`으로 감싸야 한다. `MinMax`와 `Mod7`은 각각의 이름과 같은 데이터 생성자를 제공한다.

```haskell
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)
```

이렇게 4가지 인스턴스를 정의하고 나면 다음 예제처럼 계산기를 사용할 수 있다.

```haskell
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
```

각각의 테스트 값을 `ghci`에서 입력해서 원하는 결과가 나오는지 살펴보라. 이를 통해 같은 식에 다른 의미를 얼마나 쉽게 부여할 수 있는지 알 수 있기를 바란다.

### 예제 5

스택기반 CPU에서 작동하기 위한 어셈블리 코드를 토해내는 계산기 코드를 작성하라.

하드웨어는 `StackVM.hs`에 들어있다. CPU의 명령어는 다음과 같다.

```haskell
data StackExp = PushI Integer
              | PushB Bool
              | Add
              | Mul
              | And
              | Or
                deriving Show
type Program = [StackExp]
```

`PushI`와 `PushB`는 스택에 정수와 불린값을 넣는다. `Add`와 `Mul`, `And`, `Or`은 각각 스택의 맨 위부터 2개의 원소를 꺼내서 적절한 연산을 수행한 다음 결과를 다시 스택에 넣는다. 

따라서 `[PushB True, PushI 3, PushI 6, Mul]`와 같은 프로그램을 실행하면 맨 위부터 18, True가 들어있는 스택이 생긴다.

스택에 연산에 필요한 값이 충분히 들어있지 않거나 스택에 들어있는 값의 타입이 명령어가 요구하는 타입과 일치하지 않는 경우에는 프로세서가 녹아버린다! CPU가 어떻게 돌아가는지 궁금한 독자는 [`StackVM.hs`](/files/cis194/StackVM.hs)를 보라.

산술 연산을 처리하는 컴파일러를 만들라. `Program` 타입을 위한 `Expr` 타입의 인스턴스를 만들어서 산술식을 컴파일한 프로그램 타입으로 취급할 수 있게 만들라. 정상적인 `Expr` 타입의 식인 `exp :: Expr a => a`에 대해 `stackVM exp == Right [IVal exp]`여야 한다(즉 결과값이 스택에 정수 값으로 남아야 한다).

타입 동의어(type synonym)인 `Program`의 인스턴스를 만들기 위해서는 `TypeSynonymInstances` 언어 확장을 켜야 한다. 프로그램 *맨 앞*에 다음을 추가해야 한다.

```haskell
{-# LANGUAGE TypeSynonymInstances #-}
```

이제 다음과 같은 함수를 작성하라.

```haskell
compile :: String -> Maybe Program
```

이 함수는 컴파일이 될 수 있는 문자열로 된 프로그램을 입력받은 경우 `StackVM` CPU에서 돌아갈 수 있는 프로그램을 `Just`에 넣어서 돌려준다.

### 예제 6

계산기에서 중간 결과에 이름을 붙이고, 나중에 그 이름을 활용할 있었으면 하는 고객이 있다. 

이를 위해 산술 식에 변수를 추가할 필요가 있다. 새로운 타입 클래스로 `HasVars`를 만들라. `HasVars`에 있는 유일한 메서드는 `var :: String -> a`이다. 이는 `HasVars` 타입의 인스턴스인 타입에는 변수를 표현할 수 있는 수단(그 수단의 타입이 `a`)이 존재한다는 뜻이다.

이제 `Expr`의 인스턴스인 동시에 `HasVars`의 인스턴스인 `VarExprT`를 만들자. 이를 활용하면 다음과 같은 식을 만들 수 있어야 한다.

```haskell
*Calc> add (lit 3) (var "x") :: VarExprT
```

이게 끝이 아니다. 변수를 값으로 변환해주는 매핑이 있으면, 변수가 포함된 식을 해석할 수 있어야 한다. 변수와 값 사이의 매핑을 처리하기 위해 `Data.Map` 모듈을 사용한다. 프로그램 파일 첫 부분에 다음을 추가하라.

```haskell
import qualified Data.Map as M
```

`qualified ... as M`은 항상 `M`을 앞에 붙이고 `Data.Map`에 정의된 내용을 사용하라는 뜻이다. `Prelude`와 `Data.Map`에 있는 함수의 이름이 상당수 겹치기 때문에 `qualified`를 쓴다.

다음 인스턴스를 구현하라.

```haskell
instance HasVars (M.Map String Integer -> Maybe Integer)
instance Expr (M.Map String Integer -> Maybe Integer)
```

`HasVars` 인스턴스는 변수를 `Integer` 값으로 매핑해주는 함수로 해석할 수 있다는 뜻이다. 이 인스턴스는 `Map`에 들어있는 매핑을 참조해 변수 값을 찾아야 한다.

`Expr` 인스턴스는 앞의 함수를 식을 해석할 때 쓸 수 있다는 뜻이다(이때 매핑을 하위 식을 해설하는 코드에 넘기고, 받은 결과를 적절히 조합해 처리해야 한다).

>#### 노트 
>
>이 인스턴스를 작성할 때는 `FlexibleInstances` 언어 확장을 사용해야 한다. `{-# LANGUAGE FlexibleInstances #-}`를 파일 첫 줄에 넣어야 한다. 


이 두 인스턴스를 만들고 나면 다음과 같이 코드를 테스트해볼 수 있다.

```haskell
withVars :: [(String, Integer)]
        -> (M.Map String Integer -> Maybe Integer)
        -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

*Calc> :t add (lit 3) (var "x")
add (lit 3) (var "x") :: (Expr a, HasVars a) => a
*Calc> withVars [("x", 6)] $ add (lit 3) (var "x")
Just 9
*Expr> withVars [("x", 6)] $ add (lit 3) (var "y")
Nothing
*Calc> withVars [("x", 6), ("y", 3)]
$ mul (var "x") (add (var "y") (var "x"))
Just 54
```




























