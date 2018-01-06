---
layout:     post
title:      "[하스켈 기초][CIS194] 재귀 패턴, 다형성, 프렐류드"
date:       2018-01-06 09:51:00
summary:    "실용적인 재귀 패턴을 설명하고 재귀 패턴을 추상화한 몇몇 함수를 정리한 다음, 하스켈 프렐류드에 대해 설명한다"
categories: Haskell CIS194 recursion pattern prelude
---

[하스켈 소개 연습문제 풀이](http://enshahar.com/haskell/cis194/introduction/exercise/solution/2017/12/11/cis194-introhaskell-exanswer/)와 [대수적 데이터 타입 연습문제 풀이](http://enshahar.com/haskell/cis194/algebraic/data/type/exercise/solution/2018/01/01/cis194-adt-exanswer/)를 살펴 보면 재귀함수를 직접 구현한 경우와 하스켈이 제공하는 함수를 활용하는 경우를 살펴볼 수 있다. 문제를 재귀적으로 해결하는 경우가 자주 있지만 실전 하스켈 프로그래밍에서 재귀 함수를 직접 짜는 경우는 거의 없다.

어떻게 그럴 수 있을까? 이론적으로는 무한히 많은 다양한 형태의 재귀 함수가 있을 수 있지만, 실질적으로 재귀를 사용해 문제를 풀다 보면 공통적인 패턴을 자주 보게 된다. 이런 패턴의 자세한 내용을 함수로 독립시켜 추상화하고 나면, 구체적인 재귀 구현에 신경쓰지 않고 한 단계 더 높은 층위에서 사고를 진행할 수 있다. 그렇게 한단계 더 높은 층위에서 전체적으로 생각하는 것이 *통밀 프로그래밍*이 지향하는 바다.

## 재귀 패턴

`Int`를 값으로 가지는 리스트의 정의를 다시 살펴보자.

```haskell
data IntList = Empty | Cons Int IntList
  deriving Show
```

`IntList`에 대해 수행할 수 있는 작업에 어떤 것들이 있을까? 몇가지 일반적인 작업을 꼽아보면 다음과 같다.

- 리스트의 모든 원소에게 정해진 연산을 적용한다
- 리스트의 각 원소를 검사해서 일부 원소만 남겨두고 불필요한 원소들을 제거한다
- 리스트의 모든 원소를 어떤 방법으로든 "요약"한다(모든 원소의 합계, 모든 원소의 곱, 평균, 최댓값 등등)
- 다른 일반적인 작업 패턴이 있을지 생각해 보라!

### `map` - 리스트의 모든 원소에게 정해진 연산을 적용한다

리스트의 모든 원소를 1씩 증가시켜보자. 일부러 `(+)`를 일반 함수처럼 전위 연산자로 사용했다.

```haskell
addOneToAll :: IntList -> IntList
addOneToAll Empty       = Empty
addOneToAll (Cons x xs) = Cons ((+) x 1) (addOneToAll xs)
```

리스트의 모든 원소에 대해 절대값을 구해서 리스트를 양수의 리스트로 변환할 수 있다.

```haskell
absAll :: IntList -> IntList
absAll Empty       = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)
```

또는 모든 원소를 제곱해서 양수의 리스트로 만들 수도 있다. 여기서도 일부러 `(*)`를 일반 함수처럼 사용했다.

```haskell
squareAll :: IntList -> IntList
squareAll Empty       = Empty
squareAll (Cons x xs) = Cons ((*) x x) (squareAll xs)
```

여기서 머릿 속에 큰 전구가 켜진 독자도 있을 것이다. 세 함수는 모두 같은 구조를 공유한다. 이런 공통 구조를 별도로 뽑아낼 수 있다면 굳이 같은 짓을 매번 다시할 필요가 없을 것이다.

세 예제의 공통 부분을 어떻게 뽑아내고 서로 다른 부분을 어떻게 구체화할 수 있을까? 서로 다른 부분은 바로 `(+)`, `abs`, `(*)`와 같이 각 원소에 적용할 *함수*다. 각 함수의 타입은 `Int -> Int`다. 여기서 함수를 함수에 인자로 전달하는 기능이 유용하게 쓰일 수 있다.

이제 공통점을 뽑아내서 `mapIntList`라는 함수를 만들었다고 가정하면, 다음과 같이 `addOneToAll`, `absAll`, `squareAll`을 만들 수 있다.

```haskell
exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

addOne x = x + 1
square x = x * x

mapIntList addOne exampleList
mapIntList abs    exampleList
mapIntList square exampleList
```

잠시 종이를 꺼내거나 하스켈 코딩에 쓰는 에디터를 열고, `mapIntList`를 어떻게 구현할지 생각해 보라. 

#### `mapIntList` 구현

일단 타입을 먼저 생각하는 습관을 들이자. `mapIntList`는 각 원소에 적용할 `Int->Int` 타입의 함수를 첫번째 인자로 받고, `IntList`를 두번째 인자로 받아서, 두번째 인자의 모든 원소에 첫번째 인자를 적용한 결과를 새 원소로 하는 `IntList`를 반환한다.

```haskell
mapIntList :: (Int -> Int) -> IntList -> IntList
```

앞의 세 함수 모두 공통적으로 리스트가 비어있을 때는 빈 리스트를 반환한다. 첫번째 인자인 함수는 아무 역할도 하지 않는다.

```haskell
mapIntList _ Empty = Empty
```

앞의 세 함수를 살펴보면 리스트가 `Cons`일 경우에는 `Cons`의 첫번째 인자(`Int` 타입)에 대해 원하는 함수를 적용하고, 두번째 인자에 대해 재귀적으로 작업을 수행한 다음, 그 두 결과를 `Cons`로 묶는다. 

```haskell
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)
```

구현을 모두 합치면 다음과 같다.

```haskell
-- mapIntList.hs
data IntList = Empty | Cons Int IntList
    deriving Show

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

addOne x = x + 1
square x = x * x

exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

example1 = mapIntList addOne exampleList
example2 = mapIntList abs    exampleList
example3 = mapIntList square exampleList

main = do
        print example1;
        print example2;
        print example3
```

`ghc`를 설치하면 따라오는 `runhaskell`을 사용해 이 파일을 실행할 수 있다. `runhaskell`은 `main`이라는 함수를 실행한다. `do`는 나중에 배우겠지만 `print`문을 명령형 언어에서처럼 순서대로 실행하려면 `do`를 써야 하고, 각 문장 사이를 `;`로 구분해야 한다고만 기억해 두자(이 블로그를 작성하는 나도 이에 대해 자세한 부분은 아직 잘 모른다). `;`를 없애도 똑같은 결과를 얻을 수 있지만 순차적 실행이므로 예전 명령형 언어 기분을 내기 위해 여기서 굳이 `;`를 썼다(이 또한 `;`가 있는 것과 없는 것이 정말 차이가 있는지 없는지 아직 나는 잘 모른다).

```haskell
E:\blog\example\haskell\cis194\03_recursion
λ runhaskell mapIntList.hs
Cons 0 (Cons 3 (Cons (-5) Empty))
Cons 1 (Cons 2 (Cons 6 Empty))
Cons 1 (Cons 4 (Cons 36 Empty))
```

결과는 예상대로다.

### `filter` - 검사 결과에 따라 원소 걸러내기

리스트에서 양수만 남겨보자.
```haskell
keepOnlyPositive :: IntList -> IntList
keepOnlyPositive Empty = Empty
keepOnlyPositive (Cons x xs)
  | x > 0    = Cons x (keepOnlyPositive xs)
  | otherwise = keepOnlyPositive xs
```

리스트에서 짝수만 남겨 보자.

```haskell
keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
  | even x    = Cons x (keepOnlyEven xs)
  | otherwise = keepOnlyEven xs
```

일반적인 패턴을 `filterIntList`로 뽑아낼 수 있겠는가? `filterIntList`를 사용해 `keepOnlyPositive`과 `keepOnlyEven`를 작성해 보라. 연습문제로 남겨둔다.

### `fold` - 리스트 전체를 한 값으로 요약하기

마지막은 리스트 전체를 한 값으로 요약하는 패턴으로, 보통 접기(fold)나 축약(reduce)이라 부른다. 다음 강의에 다룰 내용이라 여기서 따로 설명하지는 않겠지만 한번 어떻게 이런 패턴을 구현할 수 있을지 고민해 보라.

## 다형성

`Int` 리스트를 처리하는 `map`, `filter` 등의 멋진 고차함수를 작성했지만, `Integer`나 `Bool`로 이뤄진 리스트를 처리하는 `map`이나 `filter`가 필요하다면 어떻게 해야 할까? 또는 리스트의 리스트나 `String`의 스택 등을 처리해야 한다면? 이런 모든 경우를 처리하기 위한 새 데이터 타입과 새 함수를 정의해야 할 것이다. 더 황당한 것은 이런 모든 데이터 구조에 대한 `map`이나 `filter` 등의 함수 구현은 거의 동일하다는 점이다. 차이가 나는 부분은 *타입 시그니처(type signature)* 뿐이다. 이런 경우 도움이 될만한 기능이 하스켈에 있지 않을까?

물론 하스켈은 데이터 타입과 함수에 대한 *다형성(polymorphism)*을 지원한다. *다형적인*이라는 뜻의 "polymorphic"은 그리스어 πολύμορφος에서 온 말이며, "여러 형태를 지니는"이라는 뜻이다. 프로그래밍 언어에서 다형성이라는 말은 여러 타입에 대해 작동한다는 뜻이다.

### 다형적 데이터 타입

먼저 다형적인 데이터 타입을 선언하는 방법을 살펴보자.

```haskell
data List t = E | C t (List t)
```

(여기서 `E`와 `C`를 사용한 이유는 `Empty`와 `Cons`를 이미 `IntList`에서 썼기 때문이다. 하스켈에서는 모든 데이터 생성자는 모두 같은 네임스페이스 안에 있기 때문에 이름이 유일해야 한다.) 

`data IntList = ...`에서는 `IntList`라는 타입 이름 뒤에 아무 것도 없었지만 `data List t = ...`에는 `t`라는 변수가 있다. 이 변수를 *타입 변수*라고 부르며 이 타입 변수는 임의의 타입을 가리킨다. 하스켈에서는 타입 변수는 소문자로 시작해야 하며, 타입 이름은 대문자로 시작해야 한다. `List t`라는 타입은 `t`라는 타입을 파라미터로 받는 `List` 타입이라는 말이다. 다른말로 `List`가 파라미터화된 타입이라고 하기도 한다. 함수가 값을 파라미터로 받아서 다른 값을 내놓듯이, 파라미터화된 타입은 타입을 받아서 다른 타입을 만들어낸다.

어떤 타입 `t`가 주어지면 `(List t)`라는 타입은 `E`라는 (인자가 없는) 데이터 생성자와 `t` 타이의 값과 `(List t)` 타입의 값을 인자로 받는 `C` 생성자로 이뤄진다. 다음 예를 보자.

```haskell
lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)
```

### 다형적 함수

이제 `filterIntList` 함수를 더 일반화해서 다형적인 `List`에 대해 작동하도록 정의해보자. `filterIntList`에서 `Empty`를 `E`로, `Cons`를 `C`로 바꾸기만 하면 쉽게 그런 함수를 정의할 수 있다.

```haskell
filterList _ E = E
filterList p (C x xs)
  | p x       = C x (filterList p xs)
  | otherwise = filterList p xs
```

그렇다면 이 `filterList`의 타입은 어떻게 될까? `ghci`가 추론한 타입을 한번 살펴보자. 다음 파일을 만들고,

```haskell
-- FilterList.hs
data List t = E | C t (List t)

filterList _ E = E
filterList p (C x xs)
  | p x       = C x (filterList p xs)
  | otherwise = filterList p xs
```

`ghci`에서 로딩해 `:type filterList`를 해보자.

```haskell
E:\blog\example\haskell\cis194\03_recursion
λ ghci FilterList.hs
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from C:\Users\hyunsok\AppData\Roaming\ghc\ghci.conf
[1 of 1] Compiling Main             ( FilterList.hs, interpreted )
Ok, modules loaded: Main.
*Main> :type filterList
filterList :: (t -> Bool) -> List t -> List t
*Main>
```

`filterList :: (t -> Bool) -> List t -> List t`라는 타입 선언은 *모든 t라는 타입에 대해 `filterList`는 `t`에서 `Bool`로 가는 함수를 받고, `List t`를 받아서 `List t`를 결과로 반환하는 함수*라고 읽을 수 있다.

`mapIntList`를 `(List t)`에 대해 일반화한 `mapList`는 어떤 타입이 될까? 다음과 같은 타입을 생각한 사람도 있을 것이다.

```haskell
mapList :: (t -> t) -> List t -> List t
```

하지만 이는 제약이 너무 심한 것이다. 입력으로 받은 리스트의 원소 타입과 반환하는 리스트의 원소 타입이 꼭 같을 필요는 없다. 따라서 다음과 같이 타입을 정의하는 것이 가장 일반적인 `mapList`의 타입이 될 수 있을 것이다.

```haskell
mapList :: (a -> b) -> List a -> List b
mapList _ E        = E
mapList f (C x xs) = C (f x) (mapList f xs)
```

실제로 타입을 지정하지 않고 ghci에게 타입 추론을 맡겨도 마찬가지 타입이 나온다.

이렇게 정의된 다형적 함수를 사용할 때, 함수 정의 쪽이 아니라 **함수를 사용(호출, 적용)하는 쪽에서 타입을 선택**한다는 점이 중요하다. 다형적인 함수는 모든 가능한 타입 입력에 대해 정상 작동한다. 이런 특성은 - 어떤 값의 타입을 기반으로 직접 의사결정을 내리는 방법을 제공하지 않는다는 하스켈의 특성과 함께 - 나중에 설명하게 될 더 중요한 성질의 기반이 된다.

## 프렐류드

`Prelude`는 모든 하스켈 프로그램에 묵시적으로 임포트되는 표준 정의들이 들어있는 모듈이다. 한번 [프렐류드 문서](https://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html)를 간단히 살펴볼만한 가치가 있다. 프레류드가 제공하는 여러 도구에 익숙해지도록 하라.

물론 `Prelude`안에 다형적인 리스트 선언이 이미 들어있다. 그리고 [리스트를 다루는 다양한 다형적인 함수](https://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#g:13)들도 들어있다. 예를 들어 `filter`나 `map`은 우리가 본 `filterList`와 `mapList`에 대응한다. 실제 [Data.List](https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-List.html) 패키지에는 더 많은 다양한 유용한 리스트 함수들이 들어있다.

유용한 다형적 타입 중 하나로 `Maybe`를 들 수 있다.

```haskell
data Maybe a = Nothing | Just a
```

`Maybe a`라는 타입의 값은 내부에 `a`타입의 값이 들어있거나(그런 경우 `a`타입의 값은 `Just` 생성자로 둘러싸여 있다), `Nothing`이다. `Nothing`은 오류나 실패 등으로 인해 값이 없음을 의미한다. [Data.Maybe](https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-Maybe.html)에는 `Maybe a` 타입을 다룰 수 있는 다양한 함수가 들어있다.

### 전역 함수와 부분함수

`[a] -> a`라는 다형성 타입을 생각해 보자. 이런 타입의 함수로는 어떤 것들이 있을까? 이런 타입의 함수는 `a` 타입의 원소를 모아둔 리스트에 대해 어떤 `a` 타입의 값을 만들어내야 한다. 예를 들어 `Prelude`의 `head` 함수가 이런 타입의 함수다.

하지만 `head`에 빈 리스트가 들어가면 어떤 일이 벌어질까? `head`의 [소스 코드](https://hackage.haskell.org/package/base-4.10.1.0/docs/src/GHC.List.html#head)를 한번 살펴보라.

`head`에 빈 리스트가 들어가면 `badHead`라는 에러가 발생하면서 프로그램이 중단된다! `head`는 모든 타입에 대해 정상 작동해야 하기 때문에 이런 경우 `head`가 취할 수 있는 선택은 오류를 발생시켜 프로그램을 중단시키는 것 뿐이다. 빈 리스트를 보고 어떤 타입의 원소를 내놓아야 할지 결정할 수 없기 때문이다. 

`head`와 같은 함수를 *부분 함수(partial function)*라고 부른다. `head`의 인자 타입에 속한 값 중 일부를 `head`가 처리할 수 없기 때문에 *(인자값 중 일부에 대해서만) 부분(적으로 정의된) 함수*라는 말에서 부분 함수라는 말이 나왔다. 반대로 모든 인자 값에 대해 잘 정의된 함수를 *전역 함수(total function)*라고 부른다.

하스켈에서 가능하면 부분함수를 피해야 한다. 물론 다른 언어에서도 부분함수를 덜 사용하는 편이 좋다. 하지만 다른 언어의 경우 부분함수를 전혀 사용하지 않는 것은 상당히 성가신 일이다. 하지만 하스켈에서는 부분함수를 사용하지 않고 코딩하기 상당히 편하다.

**`head`는 실수다!** 하스켈 `Prelude`에 `head`가 들어가서는 안돼는 것이었다. `Prelude`에 들어있는 다른 부분함수로는 `tail`, `init`, `last`, `(!!)`이 있다. 이들을 가능하면 사용하지 말아야 한다. 앞으로 이런 함수를 숙제에 사용하면 코딩 스타일 관련 점수를 깎을 것이다!

그렇다면 대신 어떤 함수를 써야 할까?

#### 부분 함수 대치하기

`head`, `tail`등의 함수를 패턴 매칭으로 대신할 수 있는 경우가 많다. 다음 두 정의를 비교해보자.

```haskell
doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + (head (tail xs)) 

doStuff2 :: [Int] -> Int
doStuff2 []        = 0
doStuff2 [_]       = 0
doStuff2 (x1:x2:_) = x1 + x2
```

이 두 함수는 정확히 같은 결과를 내놓고 두 함수 모두 전역 함수다. 하지만 두번째 함수는 *명확히* 전역 함수이며 어떤 일을 하는지 읽기도 쉽다.

#### 부분 함수 작성하기

그렇다면 부분함수를 직접 *작성해야* 하는 경우가 생긴다면 어떻게 해야 할까? 두가지 방법이 가능하다. 첫번째는 함수의 출력 타입을 실패를 표현할 수 있는 타입으로 바꾸는 것이다. 대표적으로 앞에서 잠시 소개한 `Maybe`가 그런 타입이다.

내가 `head` 함수를 직접 작성해야 한다면 다음과 같이 좀 더 안전하게 작성했을 것이다.

```
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
```

실제 `safe` 패키지에 보면 [이와 똑같은 일을 하는 `headMay` 함수](https://hackage.haskell.org/package/safe-0.3.15/docs/Safe.html)가 있다.

이런 접근 방법에는 어떤 이점이 있을까?

1. `safeHead`는 절대 프로그램을 중단시키지 않는다.
2. `safeHead`의 타입을 보면 입력 중 일부에 대해 `safeHead`가 제대로 작동하지 않는다는 사실을 알 수 있다.
3. 타입 시스템이 `safeHead`의 반환 값을 적절히 검사해 `Nothing`을 제대로 처리하게 강제해준다.

어떤 의미에서는 `safeHead`도 *부분*함수다. 하지만 그 성질을 타입 시스템에 반영시켰다는 점이 원래의 `head`와는 다르다. 목표는 타입을 사용해 함수의 동작에 대해 가능한 많은 정보를 제공하는 것이다.

좋다. 그렇다면 항상 빈 리스트가 들어오지 않음을 *보장하는* 경우에만 `head`를 사용한다면 어떨까? 그런 경우 `Maybe a`를 사용해야 한다면 절대로 벌어질 수 없다는 사실을 이미 알고 있는데도 굳이 `Nothing`을 고려해야 한다는 점에서 짜증날 것이다.

답은 실제 그런 조건이 *보장*된다면 타입에서 그런 보장을 표현해야 한다는 것이다! 타입에 그 사실을 보장하면 컴파일러가 여러분을 대신해 그런 보장을 제대로 검사해준다. 예를 들어,

```haskell
data NonEmptyList a = NEL a [a]

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel []     = Nothing
listToNel (x:xs) = Just $ NEL x xs

headNEL :: NonEmptyList a -> a
headNEL (NEL a _) = a

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ as) = as
```

아마도 여러분 같은 코딩 천재에게는 이런 코드가 불필요하다고 생각할 지도 모른다. 물론 *여러분*이라면 비어있지 않은 리스트를 받아야만 하는 함수에 빈 리스트를 넘기는 것 같은 실수를 결코 저지르지 않을 것이다. 그렇지 않은가? 하지만 함께 일하는 사람 중에는 얼간이가 있기 마련이다. 물론 그 얼간이는 지금 여러분이 생각하는 그 사람은 아니다!

## 연습문제 - 코드 골프

다음 세가지 작업을 수행하면서 지정한 타입과 이름을 만족하는 하스켈 함수를 작성하라. 이때 가능한 짧게 코드를 작성하라.

- `import`나 커맨트는 코드 길이에서 제외한다.
- 커맨트로 해법을 잘 설명해야 한다.
- 공백은 코드 길이에서 제외한다. 따라서 코드를 공백을 사용해 보기 좋게 작성하라.
- `Golf.hs`라는 파일로 숙제를 제출하라. 맨 앞에는 `module Golf where`가 들어가야 한다.

> ##### 힌트
> 
> - 표준 라이브러리 함수를 최대한 많이 사용하라
> - 재귀를 직접 작성하지 말고 해당 패턴을 추상화한 표준 라이브러리 함수를 사용하라
> - 먼저 길이를 신경쓰지 말고 해법을 도출한 다음에 그 해법을 짧게 다듬어라

### 연습문제 1 - 홉스코치(Hopscotch)

다음 함수를 작성하라.

```haskell
skips :: [a] -> [[a]]
```

출력은 리스트의 리스트다. 출력 리스트의 첫번째 원소인 리스트는 입력 리스트와 같아야 한다. 두번째 리스트는 입력 리스트에서 매 두번째 원소만을 남긴 것이어야 한다. 세번째 리스트는 입력 리스트에서 매 3번째 원소만을 남긴 것이어야 한다. `n`번째 리스트는 입력 리스트에서 `n`번째 원소만을 남긴 것이어야 한다. 따라서 리스트의 길이는 입력 리스트의 길이와 같아야 한다.

```haskell
skips "ABCD" == ["ABCD", "BD", "C", "D"]
skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
skips [1] == [[1]]
skips [True,False] == [[True,False], [False]]
skips [] == []
```

### 연습문제 2 - 지역적 최댓값(local maxima)

어떤 리스트에서 *지역적 최댓값*은 자기 직전과 직후의 원소보다 더 큰(같으면 안됨) 원소들을 말한다. 예를 들어 `[2,3,4,1,5]`의 경우 `4`만 지역적 최댓값이다. `5`는 아쉽지만 자신의 뒤에 오는 값이 없기 때문에 지역적 최댓값이 아니다.

다음 `localMaxima` 함수를 정의하라.

```haskell
localMaxima :: [Integer] -> [Integer]
```

예를 들면 다음과 같은 결과가 나와야 한다.

```haskell
localMaxima [2,9,5,6,1] == [9,6]
localMaxima [2,3,4,1,5] == [4]
localMaxima [1,2,3,4,5] == []
```

### 연습문제 3 - 히스토그램

다음과 같은 함수를 작성하라.

```haskell
histogram :: [Integer] -> String
```

이 함수는 `0`부터 `9` 사이의 `Integer`의 리스트를 입력받아서 각 수가 리스트 안에 몇개씩 들어있는지 보여주는 수직 히스토그램을 출력한다. 입력에 `0`부터 `9`까지의 모든 수가 다 들어가 있지 않을 수도 있다. 출력은 아래 예제와 똑같은 형식이어야 한다.

```haskell
histogram [1,1,1,5] ==

 *
 *
 *   *
==========
0123456789

histogram [1,4,5,4,6,6,3,4,2,4,9] ==

    *
    *
    * *
 ******  *
==========
0123456789
```

> ##### 노트
> 
> `ghci`에서 `histogram [3,5]`를 실행하면 `"   * *    \n==========\n0123456789\n"`와 같은 결과를 볼 수 있을 것이다. 이는 `String` 타입의 결과 값을 *표현(representation)*한 것이다. 실제 이 문자열을 시각적으로 보기 위해서는 `putStr`을 추가해야 한다. 즉, `putStr (histogram [3,5])`라고 해야 한다.













