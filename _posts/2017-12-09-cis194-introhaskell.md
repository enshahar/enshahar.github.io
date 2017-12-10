---
layout:     post
title:      "[하스켈 기초][CIS194] 하스켈 소개"
date:       2017-12-09 22:47:00
summary:    "[하스켈기초][CIS194] 하스켈 소개 및 기본 문법 정리"
categories: Haskell CIS194 Introduction
---

CIS194 1강을 정리/번역합니다.

## 하스켈이란?

- 지연계산(lazy), 함수형(functional), 정적 타입 지정(statically typed) 프로그래밍 언어
- 1980년대 프로그래밍 언어 연구자들이 만든 위원회에서 정의한 언어(아니! 위원회에서 정의했는데 선한게 나올리가..)
- 기본적으로는 함수형 프로그래밍 언어 연구에 필요한 공통 언어역할을 하기 위해 좋은 아이디어를 모아서 만든 언어

### 함수형 언어?

- 함수(function): 수학시간에 배운 함수(순수함수). 내부를 모르는 블랙박스. 입력에 따라서만 출력이 결정됨(외부 환경 등에 의해 출력이 바뀌는 일이 없음)
- 함수형 언어라고 할때는 보통 두가지를 염두에 두는 것임
    - 함수가 1급 시민(first class): 함수를 일반 값과 똑같이 다룰 수 있다. 함수에 이름을 붙이거나(=변수에 저장하거나), 함수를 함수에서 반환하거나, 함수를 인자에 넘기거나 할 수 있다.
    - 명령을 실행한다는 개념보다는 식(expresison)의 값을 구한다(evaluation)는 개념을 중심으로 프로그램이 구성된다.

### 순수함수?

하스켈 식은 항상 참조 투명(referentially transparent)함. 이는 다음과 같은 뜻임.
- 상태 변경(mutation)이 없다. 모든 것(변수, 데이터 구조 등등)이 불변(immutable)이다.
- 식은 결코 부수효과(side effect, 전역 변수를 갱신하거나 I/O를 발생시키는 등의 일)를 발생시키지 않는다.
- 같은 함수에 같은 인자를 넘기면 항상 같은 결과를 얻는다.

미쳤나? 어떻게 상태 변경과 부수효과가 없이 코딩이 가능할까? => 생각하는 방법을 바꿀 필요는 확실히 있다. 하지만 잇점도 있다.

- 동등성에 의한 추론과 리팩터링이 가능함. 하스켈에서는 어떤 식/값을 다른 동등한 식/값으로 언든지 바꿀 수 있다. 산술 계산시 등호(`=`)의 좌항과 우항의 값을 서로 바꿔써도 무방한 것과 같다.
- 병렬성 확보 가능. 식이 많이 있어도 서로 영향을 끼치지 못하기 때문에 원하는데로 병렬로 실행해도 된다.
- 머리가 덜 아픔. 부수 효과가 없어서 전역변수나 상태 변경으로 인해 겪는 어려움이나 내가 모르는 어떤 곳에서 생긴 변화가 내 코드에 영향을 끼치는 일이 없으므로 디버깅이나 유지보수나 프로그램의 성질을 추론하기가 더 쉽다.

### 지연?

 하스켈에서는 각 식의 값이 실제로 필요하기 전까지는 그 식을 계산하지 않는다. 결정 자체는 단순하지만 그로 인한 파급효과는 엄청나다. 이 과목을 배우는 동안 이에 대해 계속 살펴볼 것이다. 그런 파급 효과를 몇가지 여기서 이야기하자면:

 - 함수를 선언해서 새로운 제어 구조를 만들기 쉽다(이게 lazy와 무슨 관계일까요? 모르겠습니다 T.T).
 - 무한한 데이터 구조를 정의하고 사용할 수 있다.
 - 더 합성성(compositionality)이 좋아진다. 
 - 단점으로 프로그램의 시간/공간 사용 패턴을 이해하기가 어려워진다.

### 정적 타입 지정?

 - 모든 하스켈 식에는 타입이 있으며, 그 타입은 컴파일 시점에 검사된다. 타입 검사를 통과하지 못하는 프로그램은 컴파일될 수 없다.

## 본 강의가 다룰 주제

다음 세가지 주제를 본 강의에서 주로 다룬다.

### 타입

정적 타입 지정 시스템을 보면 짜증이 날 때가 많다. 자바, C++ 등을 보면 실제로 그렇다. 이는 정적 타입 지정 자체의 문제라기 보다는 자바, C++ 타입 시스템의 표현력이 떨어져서다. 이 강의에서는 하스켈 타입 시스템을 자세히 살펴본다. 하스켈 타입 시스템의 특징은 다음과 같다.

- 하스켈 타입 시스템은 생각을 명확히 하고 프로그램 구조를 깔끔하게 표현할 수 있게 도움을 준다: 하스켈 프로그램을 작성할 때 가장 먼저 해야 할 일은 타입을 정리하는 것이다. 하스켈 타입 시스템이 복잡하기 때문에 타입을 정리하는 작업 자체가 간단하지 않은 설계 과정이라 할 수 있고, 이렇게 타입을 정하는 과정은 작성할 프로그램에 대한 생각을 깔끔하게 정리하는 과정이기도 하다.
- 프로그램에 대한 문서 역할을 한다: 타입 시스템의 표현력이 좋다면 함수의 타입만 봐도 그 함수가 어떤 일을 하고 어떻게 그 함수룰 활용할 수 있을지 쉽게 추측할 수 있다.  
- 실행 시점 오류를 컴파일 시점 오류로 바꿔줄 수 있다: 테스트를 많이 작성해서 프로그램의 문제를 걸러내기를 희망하는 대신, 타입을 잘 설계해서 가능한 오류를 최대한 컴파일시 걸러낼 수 있다. "컴파일만 되면 제대로 작동해"라고 말하는 것은 보통은 건방진 말이지만 하스켈에서는 이 말이 성립될 가능성이 다른 언어에 비해 더 높다.

### 추상화

"반복하지 말라(Don't Repeat Yourself)"라는 말을 자주 들었을 것이다. 또는 "추상화 원칙(Abstraction Principle)"이라는 말을 들었을 수도 있다. 이는 모든 반복을 제거하자는 말이다. 즉, 어떤 생각이나 알고리즘, 데이터 구조의 일부 등이 코드 안에 단 한번만 나타나게 하자는 이야기다. 비슷한 코드들을 모아서 유사한 부분을 하나로 묶어내는 과정을 추상화라고 부른다.

하스켈은 추상화를 하기 아주 좋은 언어다. 파라미터화한 다형성(parametric polymorphism), 고차 함수(high-order function), 타입 클래스 등이 중복과 싸울 때 큰 도움이 된다(어려운 단어에 기죽지 말자! 나중에 나오겠지!). 

### 통밀 프로그래밍

이 강의를 통해 통밀(wholemeal) 프로그래밍을 살펴본다. 랄프 하인즈(Ralf Hinze)의 글을 인용한다. 

> 함수형 프로그래밍 언어는 통밀 프로그래밍에 쓰기 훌륭하다. 통밀 프로그래밍이라는 용어는 제레인트 존스(Geraint Jones)가 만든 말로, 프로그램을 통크게 작성하는 것이다. 각 원소를 순서대로 처리한다고 생각하는 대신 시퀀스 전체를 처리한다고 생각하고, 각 해법을 하나하나 찾아내는 대신 해법의 공간을 만들어내며, 단일 경로(path)를 상상하는 대신 그래프(graph) 전체를 상상한다. 통밀 프로그래밍을 사용하면 주어진 문제에 대해 새로운 직관을 얻거나 새로운 관점을 얻을 수 있는 경우가 자주 있다. 이 생각을 프로젝션을 활용한 프로그래밍(projective programming)이라는 아이디어로 보충할 수 있다. 먼저 더 일번적인 문제를 풀고, 그렇게 푼 일반적인 프로그램을 특화함으로써 관심이 있는 부분을 추출해낸다.

C나 자바같은 언어로 된 다음 의사코드(pseudo-code)를 보자.

```Java
int acc = 0;
for ( int i = 0; i < lst.length; i++ ) {
  acc = acc + 3 * lst[i];
}
```

이 코드는 너무 "인덱스적"이다. 즉, 현재 인덱스를 유지하면서 배열을 이터레이션하는 낮은 수준의 작업에 관심을 집중하고 있다. 게다가 배열의 각 원소를 3배로 만들고, 결과를 합치는 2가지 유용한 연산이 서로 뒤섞여있다.

하스켈에서는 

```haskell
sum (map (3*) lst)
``` 

이라고 쓸 수 있다.

## 하스켈 언어 기초

여기서는 기본적인 하스켈 문법을 다룬다. 특별히 이야기하지 않으면 `.hs`로 확장자가 끝나는 하스켈 파일에 저장해 하스켈 컴파일러로 컴파일하거 ghci에서 `:load 파일이름` 으로 파일을 로딩해 테스트할 수 있다. ghci에서 `:edit 파일이름`으로 파일을 에디팅할 수도 있다. 여기까지 본 사람은 ghci의 특별 명령이 `:`로 시작된다는 사실을 알 수 있을 것이다. 

`:edit 파일이름`을 했을 때 실행될 에디터는 `:set editor 명령이름`과 같이 지정할 수 있다. 매번 ghci를 시작할 때마다 `:set`을 사용하기는 번거롭기 때문에,  ghci 설정 파일에 `:set editor`를 정해두면 좋다. ghci 설정 파일은 여러 곳에 있을 수 있고, 다음 순서로 처리된다.

1. `./.ghci`
2. `appdata/ghc/ghci.conf` (여기서 *appdata*는 보통 *C:/Documents and Settings/사용자이름/Application Data*)
3. `$HOME/.ghc/ghci.conf`
4. `$HOME/.ghci`

### 주석과 선언, 변수

`--` 다음부터 줄 끝까지 오는 내용은 모두 주석이다. 여러 줄 주석이 필요하다면 `{-`와 `-}` 사이에 넣으면 된다. 

한가지 흥미로운 점은 `{-`,`-}` 짝이 맞아야 한다는 것이다. 여는 `{-`는 있는데 `-}`가 없거나, 닫는 `-}`가 여는 `{-` 보다 더 많으면 오류다. 닫는 `-}`가 없으면 `unterminated {-`라는 오류가 나고, 닫는 `-}`가 더 있으면 파싱 오류(`parse error on input }`)가 난다.

변수 선언은 특별한 키워드 없이 `변수이름 = 식` 같은 형태를 취한다. 원한다면 `변수이름 :: 타입`과 같은 형태로 변수의 타입을 선언할 수도 있다. 지금까지 설명한 것을 파일에 넣어보면 다음과 같다.

```haskell
-- declaration.hs

{- 여러줄 커맨트
  {- 여러줄 커맨트 2단계 -}
-}
-- 한줄 커맨트

x :: Int  -- 타입을 선언한다
x = 10    -- 값을 지정한다

y = x * 2  -- 타입 선언 없이 값만 지정한다
```

`ghci declaration.hs`하면 `*Main>`이라는 프럼프트가 나오는데 거기서 `x`와 `y`의 값을 살펴볼 수 있다.

```haskell
*Main> x
10
*Main> y 
20
```

하스켈에서 이렇게 정의한 변수는 실제로는 값이 들어있는 어떤 박스(또는 메모리)가 아니다. 그냥 값에 이름을 부여한 것이다. 따라서 `x=10`을 `x`에 `10`을 대입한다(assign 4 to x)고 읽거나 생각하면 안되고, x가 10이라고 정의한다(x is defined to be 4)라고 읽어야 한다.

> ##### 생각해볼 문제
>
> 다음 코드는 어떤 뜻일까?
>
> ```haskell
> y :: Int
> y = y + 1
> ``` 

### 기본 타입
하스켈 기본 타입은 `Int`, `Integer`, `Double`, `Float`, `Bool`, `Char`, `String`이 있다. 

- `Int`는 최소 `-2^29`부터 `+2^29`의 범위를 보장하는 정수타입이다. 구현(컴퓨터 아키텍처)에 따라 가능한 값의 범위가 달라질 수 있다. 
- `Integer`는 값의 한계가 없는 정수타입이다. 
- `Double`과 `Float`는 각각 배정밀도, 단정밀도 부동소수점 수 타입이다.
- `Bool`은 `True`와 `False`를 유이한 값으로 가지는 불리언 타입이다.
- `Char`는 유니코드 문자를 표현하는 타입이다.
- `String`은 `Char`의 리스트(리스트에 대해서는 나중에 다룸)지만, 문자열 표현을 허용한 타입이다.

```haskell
-- types.hs
-- 기계에 따라 범위가 정해지는 정수 타입
i :: Int
i = -78

biggestInt, smallestInt :: Int
biggestInt  = maxBound
smallestInt = minBound

-- 임의 크기 정수
n :: Integer
n = 1234567890987654321987340982334987349872349874534

reallyBig :: Integer
reallyBig = 2^(2^(2^(2^2)))

numDigits :: Int
numDigits = length (show reallyBig)  -- length, show에 대해서는 나중에.

-- 배정밀도 부동소수점 수
d1, d2 :: Double
d1 = 4.5387
d2 = 6.2831e-4

-- 불리언
b1, b2 :: Bool
b1 = True
b2 = False

-- 유니코드 문자
c1, c2, c3 :: Char
c1 = 'x'
c2 = 'Ø'
c3 = 'ダ'

-- 문자열
s :: String
s = "Hello, Haskell!"
```

ghci로 이 코드를 테스트해보면 다음과 같다.

```haskell
E:\blog\example\haskell\cis194\01_intro
λ ghci types.hs
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from C:\Users\hyunsok\AppData\Roaming\ghc\ghci.conf
[1 of 1] Compiling Main             ( types.hs, interpreted )
Ok, modules loaded: Main.
*Main> biggestInt
9223372036854775807
*Main> numDigits
19729
*Main> d1
4.5387
*Main> d2
6.2831e-4
*Main> c3
'\12480'
*Main> s
"Hello, Haskell!"
*Main>
```

> ##### 이름 짓는 관례
>
> 하스켈에서는 일반적으로 캐멀케이스(`camelCase`)를 사용한다.

### ghci 

ghci 사용법을 하나 더 알려준다. `:load 파일이름`하면 파일을 읽어서 그 파일 안에 있는 정의를 사용할 수 있게 해준다. 읽은 파일을 변경한 경우 다시 읽으려면 `:reload`를 하면 된다. `:load 파일이름`과 `:reload`는 각각 `:l 파일이름`과 `:r`로 짧게 쓸 수 있다.

값, 변수, 식의 타입을 알고 싶으면 `:type 식/값/변수`이나 `:t 식/값/변수`와 같이 입력하면 된다. 

명령어에 대한 도움말은 `:?`를 치면 볼 수 있다.

### 산술연산

일반적인 산술 연산을 사용할 수 있다. ghci에서 다음을 테스트해보자. `Prelude>`라는 프럼프트는 아무 파일도 로딩 안하고 ghci를 실행하면 나타나는 프럼프트다.

```haskell
E:\blog\example\haskell\cis194\01_intro
λ ghci
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from C:\Users\hyunsok\AppData\Roaming\ghc\ghci.conf
Prelude> ex01 = 3 + 2
Prelude> ex02 = 19 - 27
Prelude> ex03 = 2.35 * 8.6
Prelude> ex04 = 8.7 / 3.1
Prelude> ex05 = mod 19 3
Prelude> ex06 = 19 `mod` 3
Prelude> ex07 = 7 ^ 222
Prelude> ex08 = (-3) * (-7)
Prelude> ex01
5
Prelude> ex02
-8
Prelude> ex03
20.21
Prelude> ex04
2.8064516129032255
Prelude> ex05
1
Prelude> ex06
1
Prelude> ex07
40903915558252355961885564235233827390274916808670721972378015470397485101670867316479654900404204284975885535566242786061025593172032118590958393531614633803778811048702555046770492868049
Prelude> ex08
21
Prelude>
```

몇가지 특이한 점이 있다.

- `mod`를 호출할 때 `mod 19 3`처럼 `함수이름 인자1 인자2`를 사용한다.
- `mod`를 `mod 19 3`처럼 사용할 때는 `mod`라는 이름을 그대로 사용하지만 ```19 `mod` 3``` 처럼 사용할 때는 백틱("`")으로 감싼다.
- 음수를 귀찮게 괄호("()")로 감싼다.

`mod`와 같이 알파벳으로 시작하는 함수를 중위 표기로 사용하려면 백틱을 사용해야 하고, `-`를 뺄셈 연산자가 아닌 음수 기호로 사용하기 위해서는 괄호로 감싸서 파싱 오류를 방지해야 한다. `-`의 경우 못생겼지만 어쩌겠는가?

한편 다음은 오류가 난다.

```haskell
E:\blog\example\haskell\cis194\01_intro
λ ghci types.hs
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from C:\Users\hyunsok\AppData\Roaming\ghc\ghci.conf
[1 of 1] Compiling Main             ( types.hs, interpreted )
Ok, modules loaded: Main.
*Main> i+n

<interactive>:1:3: error:
    ? Couldn't match expected type ‘Int’ with actual type ‘Integer’
    ? In the second argument of ‘(+)’, namely ‘n’
      In the expression: i + n
      In an equation for ‘it’: it = i + n
*Main>
```

`i`는 `Int`, `n`은 `Integer`인데 `Int`를 자동으로 더 범위가 큰 값인 `Integer`로 변환해서 계산해 주지는 않는다. 이런 경우 다음과 같은 수단을 사용해 명시적으로 타입을 변경해야 한다.

- `fromIntegral`: 정수 타입(`Int`또는 `Integer`)을 다른 수 타입으로 변환해준다.
- `round`, `floor`, `ceiling`: 부동소수점 수를 `Int`나 `Integer`로 변환해준다.

따라서 위 `i+n`은 다음과 같이 해야 한다.

```haskell
*Main> (fromIntegral n) + n
2469135781975308643974681964669974699744699749068
```

`div`는 정수 나눗셈, `/`는 부동소수점 수 나눗셈이다.

```haskell
*Main> i/i

<interactive>:3:1: error:
    ? No instance for (Fractional Int) arising from a use of ‘/’
    ? In the expression: i / i
      In an equation for ‘it’: it = i / i
*Main> i `div` i
1
```

### 불리언 로직

불리언 로직은 `&&`와 `||`, `not`으로 조합된다. ghci에서 테스트해보자.

```haskell
Prelude> ex11 = True && False
Prelude> ex12 = not (False || True)
Prelude> ex11
False
Prelude> ex12
False
```

`==`, `/=`로 동등성을 검사하거나, `<`,`>`,`<=`,`>=`으로 대소를 비교할 수 있다.

```haskell
Prelude> ex13 = ('a' == 'a')
Prelude> ex14 = (16 /= 3)
Prelude> ex15 = (5 > 3) && ('p' <= 'q')
Prelude> ex16 = "Haskell" > "C++"
Prelude> ex13
True
Prelude> ex14
True
Prelude> ex15
True
Prelude> ex16
True
```

#### `if` 식

`if` 식도 사용 가능하다. `if b then t else f`는 `b`가 `True`면 `t`, `False`면 `f`를 값으로 하는 식이다. 여기서 `if` 문(statement)이 아니라 `if` 식(expression)이라고 말했다는 점에 유의하라. 하스켈에서는 `if` 식을 다른 식의 부분식으로 사용할 수 있고, 항상 `else` 부분이 있어야 한다. C, C++, 자바 등의 3항 연산자 `b ? t : f`와 같다고 생각할 수 있다.

하지만 하스켈에서 `if` 문을 자주 사용할 일은 없다. 보통은 패턴 매칭(pattern matching)이나 가드(guard)를 사용해 조건을 처리한다.

### 함수 정의하기

(cis194와 조금 다르게 바꿨습니다.)

짝수면 0, 홀수면 1을 반환하는 함수를 정의해보자. 물론 `x mod 2`하면 쉽게 이런 함수를 정의할 수 있겠지만, `if` 식을 한번 써보자.

```haskell
-- parity.hs
parity :: Integer -> Integer
parity n = if (n `mod` 2) == 0 then 0 else 1 -- parity n = n `mod` 2 로도 가능
```

변수 타입을 `::`를 사용해 선언한 것처럼 함수 타입도 `::`로 선언할 수 있다. `Integer -> Integer`에서 `->`는 `->` 왼쪽 타입의 값을 인자로 받아서 오른쪽 타입의 값을 결과로 돌려주는 함수라는 뜻이다.

함수를 정의할 때 함수 이름과 파라미터 변수 사이에 있는 공백만으로도 함수와 파라미터를 구분할 수 있으므로 함수 선언시 파라미터 주변을 괄호로 감쌀 필요가 없다(처음에는 이런 방식이 이상해 보일지 모르지만 나중에는 간결해 보이게 될 것이다. 또한 키보드를 덜 두들겨도 되서 좋다). 함수 이름, 인자 다음에 `=`가 오고 그 우변에 함수 본문식이 온다. 하스켈 함수 정의는 수학 함수 정의와 비슷하다.

위와 같이 정의한 함수를 ghci에서 테스트해보면 잘 작동함을 알 수 있다.

```haskell
E:\blog\example\haskell\cis194\01_intro
λ ghci parity.hs
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from C:\Users\hyunsok\AppData\Roaming\ghc\ghci.conf
[1 of 1] Compiling Main             ( parity.hs, interpreted )
Ok, modules loaded: Main.
*Main> parity 1
1
*Main> parity 11
1
*Main>
```

`if` 대신 가드를 사용할 수도 있다. `|` 뒤에 조건을 쓰고 조건 뒤에 `=`과 함수 본문을 쓰면 된다. 모든 가드를 만족하지 않을 때는 `otherwise` 조건이 사용된다.

```haskell
-- parity2.hs
parity2 :: Integer -> Integer
parity2 n
  | n `mod` 2 == 0 = 0
  | otherwise == 1 = 1
```

`if`식에 대해 이야기하면서 패턴 매칭에 대해 이야기한 적이 있다. 우리나라 깊은 산속 어딘가에 있는 종족이 수를 세는 방법을 패턴 매칭을 사용해 구현하면 다음과 같다.

```haskell
-- count.hs
count :: Integer -> String
count 0 = "뭐?" 
count 1 = "하나"
count 2 = "둘" 
count n = "많다"
```

패턴 매칭과 가드의 차이를 알 수 있겠는가? 가드와 패턴매칭을 동시에 활용할 수도 있다.

```haskell
-- foo.hs
foo :: Integer -> Integer
foo 0 = 16
foo 1 
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0            = 0
  | n `mod` 17 == 2  = -43
  | otherwise        = n + 3
```

다음 함수값이 뭐가 될지 생각해 보자.

- `foo (-3)`
- `foo 0`
- `foo 1`
- `foo 36`
- `foo 38`

정답은 다음과 같다.

```haskell
*Main> :l foo.hs
[1 of 1] Compiling Main             ( foo.hs, interpreted )
Ok, modules loaded: Main.
*Main> foo (-3)
0
*Main> foo 0
16
*Main> foo 1
3
*Main> foo 36
-43
*Main> foo 38
41
*Main>
```

함수 안에서 함수 자신을 호출할 수도 있다. 하스켈에서는 이런 재귀 함수를 자주 사용한다.

```haskell
-- sumtorial.hs
-- 1부터 n까지 정수의 합을 계산한다
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)
```

가우스가 1부터 100까지 합계를 제대로 계산했나 살펴보자.

```haskell
*Main> :l sumtorial.hs
[1 of 1] Compiling Main             ( sumtorial.hs, interpreted )
Ok, modules loaded: Main.
*Main> sumtorial 100
5050
*Main>
```

### 순서쌍(pair)

둘 이상의 값을 묶을 때는 괄호로 묶고 각각을 `,`로 구분한다. 타입도 `( 원소타입1, 원소타입2 )` 처럼 값과 똑같은 패턴으로 생겨먹었다. 다음은 `Int` 값과 `Char`값을 엮은 순서쌍 `p`를 정의한다. 순쌍을 튜플(tuple)이라고도 한다.

```haskell
p :: (Int, Char)
p = (3, 'x')
```

이제 순서쌍을 인자로 받는 함수도 정의할 수 있을 것이다. 이때 패턴 매칭을 사용해 순서쌍의 각 값을 분리해낼 수 있다. 패턴 매칭을 사용하지 않은 경우에는 

```haskell
-- sumPair.hs
sumPair :: (Int,Int) -> Int
sumPair (x,y) = x + y
```

원소가 3개인 트리플(triple), 4개인 쿼드러플(quadruple) 등을 사용하면 인자가 더 많은 함수를 정의할 수도 있다. 하지만 더 나은 방법이 있다.

### 인자가 여럿 있는 함수

인자가 많은 함수를 튜플을 사용하지 않고 정의하고 싶다면, 각 인자를 공백으로 구분해 정의하면 된다. 세 인자의 합계를 계산하는 `f`를 정의해보자.

```haskell
-- args.hs
f :: Int -> Int -> Int -> Int
f x y z = x + y + z

ex17 = f 3 17 8  -- 28
```

실제 ghci에서 `:l args.hs`를 하고 `ex17`을 평가(REPL에서 `ex17`을 입력하면 ghci가 `ex17`의 값을 표시해준다. 이 과정에서 `ex17`의 값을 계산해야 하는데 이를 평가(evaluation)이라고 말한다)하면 실제 28이 나온다.

```haskell
*Main> :l args.hs
[1 of 1] Compiling Main             ( args.hs, interpreted )
Ok, modules loaded: Main.
*Main> ex17
28
*Main>
```

`f`의 타입 `Int -> Int -> Int -> Int`는 무슨 뜻일까? 이에 대해서는 나중에 배운다. 일단은 `Int`를 순서대로 3개 받아서 맨 마지막의 `Int`를 계산해내는 함수라고 생각하자. 일반적으로 말하면 `인자타입1 -> 인자타입2 -> ... -> 인자타입n -> 결과타입`으로 인자가 `n`개인 함수의 타입을 정의할 수 있다.

함수 적용(application)은 항상 이항 연산자보다 더 우선순위가 높다는 점에 유의하라. 다음 코드는 

```haskell
n = 10
f 3 n+1 7
```

다음과 같이 해석된다.

```haskell
(f 3 n)+(1 7)
```

따라서 전혀 엉뚱한 오류 메시지가 나올 수 있다.

```haskell
*Main> n = 10
*Main> f 3 10+1 7

<interactive>:6:1: error:
    ? No instance for (Num (Int -> Int)) arising from a use of ‘+’
        (maybe you haven't applied a function to enough arguments?)
    ? In the expression: f 3 10 + 1 7
      In an equation for ‘it’: it = f 3 10 + 1 7

<interactive>:6:8: error:
    ? No instance for (Num (t0 -> Int -> Int))
        arising from the literal ‘1’
        (maybe you haven't applied a function to enough arguments?)
    ? In the expression: 1
      In the second argument of ‘(+)’, namely ‘1 7’
      In the expression: f 3 10 + 1 7

<interactive>:6:10: error:
    ? Ambiguous type variable ‘t0’ arising from the literal ‘7’
      prevents the constraint ‘(Num t0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘t0’ should be.
      These potential instances exist:
        instance Num Integer -- Defined in ‘GHC.Num’
        instance Num Double -- Defined in ‘GHC.Float’
        instance Num Float -- Defined in ‘GHC.Float’
        ...plus two others
        (use -fprint-potential-instances to see them all)
    ? In the first argument of ‘1’, namely ‘7’
      In the second argument of ‘(+)’, namely ‘1 7’
      In the expression: f 3 10 + 1 7
*Main>
```

이렇게 인자가 여럿 있는 함수와 적용 순서 등에 대해서는 나중에 더 자세히 다룬다.

### 리스트 

앞으로 하스켈에서 리스트를 사용하는 경우를 자주 볼 수 있을 것이다. 리스트는 가장 기초적인 하스켈 데이터 타입 중 하나다.

```haskell
nums, range, range2 :: [Integer]
nums   = [1,2,3,19]   -- 원소를 나열
range  = [1..100]     -- 범위로 지정
range2 = [2,4..100]   -- 원소와 범위 지정을 함께 사용
```

리스트의 타입은 `[원소타입]`처럼 `[]`로 원소의 타입을 감싸서 표현한다.

나중에 배우겠지만 리스트 컴프리핸션(comprehension)을 통해 조건제시법 비슷하게 리스트의 원소를 정의할 수도 있다. 이에 대해 [LYAH(Learn You a Haskell for Greater Good)](http://learnyouahaskell.com/starting-out)를 참조하라. 국문판은 "가장 쉬운 하스켈 책"(BJ퍼블릭)이 있다. 영어판은 유명한 책이고 좋은 책이지만, 나는 국문판을 본 적이 없으므로 혹시 국문판을 구매할 분은 서점 등에서 직접 확인후 구매하기 바란다.

하스켈 문자열은 `[Char]`에 대한 별칭이다. 따라서 `Char`의 리스트를 `String`과 비교해도 문제가 없다.

```haskell
-- hello.hs
hello1 :: [Char]
hello1 = ['h', 'e', 'l', 'l', 'o']

hello2 :: String
hello2 = "hello"

helloSame = hello1 == hello2  -- True
```

따라서 리스트를 다룰 수 있는 함수를 `String`에 적용해도 아무 문제가 없다.

#### 리스트 만들기

빈 리스트가 가장 단순한 리스트일 것이다. 

```haskell
emptyList = []
```

콘스(cons, `:`)를 사용해 빈 리스트로부터 리스트를 구축할 수 있다. 콘스는 원소와 리스트를 받아서 리스트 맨 앞에 원소를 덧붙인(prepend) 새 리스트를 반환하는 함수다. 

```haskell
ex18 = 1 : []
ex19 = 3 : (1 : [])
ex20 = 2 : 3 : 4 : []

ex21 = [2,3,4] == 2 : 3 : 4 : []
```

`[2,3,4]`라는 리스트 표현은 `2 : 3 : 4 : []`를 편리하게 짧게 쓸 수 있게 해준 것 뿐이다. 하스켈 리스트는 실제로 단일 연결 리스트로 만들어진다. 배열이 아니라는 점에 유의하라.

다음 규칙에 의해 만들어지는 시퀀스를 hailstone 시퀀스라고 부른다. (hail은 우박이라는 뜻)

- n이 1이면 시퀀스가 끝난다.
- n이 짝수면 다음은 n/2이다.
- n이 홀수면 다음은 3 * n + 1이다. 

현재 `n`에서 다음 수를 계산하는 `hailStone`이라는 함수를 만들 수 있고, 이를 사용해 실제 hailstone 시퀀스를 만들 수 있다.

```haskell
-- hailstone.hs
hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3 * n + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)
```

몇가지 시퀀스를 테스트해보자(0이나 음수를 넣으면 안된다).

```haskell
*Main> :l hailstone.hs
[1 of 1] Compiling Main             ( hailstone.hs, interpreted )
Ok, modules loaded: Main.
*Main> hailstoneSeq 10
[10,5,16,8,4,2,1]
*Main> hailstoneSeq 30
[30,15,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]
*Main>
```

#### 리스트에 작용하는 함수

패턴 매칭을 사용해 리스트에 대한 함수를 정의할 수 있다. 다음은 재귀를 사용해 리스트 길이를 계산하는 함수다.

```haskell
intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs
```

재귀함수를 정의할 때는 재귀가 종료되는 조건을 명확히 하고, 재귀를 통해 그 종료 조건으로 다가가도록 로직을 설계해야 한다. 재귀에 익숙하지 않은 경우에는 연습문제를 많이 풀거나 분할정복(divide and conquer) 알고리즘들을 살펴보면서 관련 내용에 익숙해지기 바란다.

위 함수의 첫번째 패턴은 리스트가 비어있는 경우 전체 길이가 0임을 표현한다. 

두번째 패턴은 리스트에 하나라도 원소가 있는 경우 전체 리스트 길이는 리스트의 첫번째 원소를 제외한 나머지(`xs`)의 길이(`intListLength xs`)에 1을 더한 것임을 표현한다.

이런 경우 `x`의 값을 알 필요는 없고 존재 여부에만 관심이 있으므로 `x` 대신 `_` (밑줄)을 사용해 `intListLength (_:xs) = 1 + intListLength xs`처럼 작성할 수도 있다.

패턴을 내포시킬 수도 있다.

```haskell
sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []     -- 빈 리스트의 경우 아무것도 하지 않음
sumEveryTwo (x:[])     = [x]    -- 리스트 원소가 1개뿐인 경우 아무 것도 하지 않음
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs -- 앞에서부터 2개씩 원소를 더해서 새로운 리스트를 구축함
```

여기서 `(x:(y:zs))` 대신 `(x:y:zs)`라고 써도 된다. 

### 함수 조합하기

하스켈에서는 간단한 함수를 조합해서 더 복잡한 함수를 만드는 방식을 자주 사용한다. 

앞의 `hailstoneSeq`와 `intListLength`를 조합하면 쉽게 hailstone 시퀀스의 길이를 알려주는 함수를 만들 수 있다.

```haskell
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1
```

이 코드를 보면 처음 드는 생각은 "모든 hailstone 시퀀스를 만들고 그 리스트의 길이를 재다니 너무 비효율적인거 아니야?"일 것이다. 하지만 하스켈은 지연 계산을 사용하기 때문에 `intListLength` 호출과 `hailStoneSeq` 시퀀스 생성이 서로 뒤엮여 발생한다. 따라서 전체 계산에 시퀀스 길이와 관계 없이 O(1) 메모리가 필요하다. (실제로 이는 선의의 거짓말이긴 하다. 거짓말인 이유와 실제 O(1)으로 만드는 방법에 대해서는 이 강의를 듣다 보면 알게 될 것이다.)

### 오류 메시지에 대해 한마디

오류 메시지를 무서워하지 말라. GHC의 오류 메시지는 상당히 길고 (겉보기엔) 무섭다. 하지만 메시지가 긴 이유는 많은 정보가 포함되어 있기 때문이다. 다음 예를 보자.

```haskell
Prelude> 'x' ++ "foo"

<interactive>:1:1:
    Couldn't match expected type `[a0]' with actual type `Char'
    In the first argument of `(++)', namely 'x'
    In the expression: 'x' ++ "foo"
    In an equation for `it': it = 'x' ++ "foo"
```

여기서 `Couldn't match expected type [a0] with actual type Char`는 리스트 타입인 어떤 값을 예상했는데 실제로는 `Char` 타입이 들어왔다는 이야기다. 그럼 리스트  타입이어야 할 어떤 값은 무엇일까? 다음 줄을 보면 `(++)`의 첫 인자인 `x`에서 그런 문제가 있다고 말한다. 그 뒤의 두 줄은 문맥을 좀 더 자세히 설명해준다. 

이제 `'x'`가 `Char`타입이지 `String` 타입(기억하겠지만 `String`은 `[Char]`와 같다)이 아니어서 생기는 문제임을 쉽게 알 수 있다. 

하스켈 오류 메시지를 보면 처음엔 도망가고 싶은 생각이 들 것이다. 하지만 한번 크게 숨을 들이쉬고 오류 메시지를 자세히 읽어보라. 모든 내용을 다 이해할 수는 없겠지만 무언가 그 메시지에서 배우는 내용이 있을 것이고, 문제가 무엇인지 이해할 수 있는 단서를 찾을 수 있을 것이다.


## 숙제

숙제는 [여기](http://www.seas.upenn.edu/%7Ecis194/spring13/hw/01-intro.pdf) 있습니다. 정리하면:

### 신용카드 검증

1. 오른쪽부터 매 2번째 번호를 2배로 곱한다. 예를 들어 `[1,3,8,6]`이 카드번호라면 `[2,3,16,6]`이 되야 한다.
2. 1에서 나온 모든 수의 각 숫자를 더한다. `[2,3,16,6]`은 `2+3+1+6+6`으로 계산해서 `18`이 결과다.
3. 2의 합계를 10으로 나눈 나머지를 계산한다. 따라서 `1386`이 카드번호라면 `8`이 답이다.

1~3의 절차를 거쳐 계산한 값이 0이면 카드가 맞는 카드다.

#### 연습문제 1

정수의 각 자리의 숫자를 리스트로 돌려주는 다음과 같은 함수를 정의하라.

- `toDigits    :: Integer -> [Integer]`
- `toDigitsRev :: Integer -> [Integer]`

`0`이나 음수에 대해서는 빈 리스트를 반환하라.

예제:

```haskell
toDigits 1234 == [1,2,3,4]
toDigitsRev 1234 == [4,3,2,1]
toDigits 0 == []
toDigits (-17) == []
```

#### 연습문제 2

리스트의 맨 끝을 기준으로 매 2번째마다 있는 수를 2배하는 함수를 만들라.

- `doubleEveryOther :: [Integer] -> [Integer]`

예제:

```haskell
doubleEveryOther [8,7,6,5] == [16,7,12,5]
doubleEveryOther [1,2,3] == [1,4,3]
```

#### 연습문제 3

2자리나 1자리 수가 섞인 리스트의 모든 숫자를 더하는 함수를 만들라.

- `sumDigits :: [Integer] -> Integer`

예제:

```haskell
sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
```

#### 연습문제 4

카드를 검증하는 함수를 만들라.

- `validate :: Integer -> Bool`

이 함수는 앞에서 정의한 함수들을 활용해야 한다.


예제:

```haskell
validate 4012888888881881 = True
validate 4012888888881882 = False
```

### 하노이 탑

하노이 탑 문제는 재귀 알고리즘의 대표적인 예다. 세 기둥이 있고, 그 중 어떤 한 기둥에 크기가 다른 여러 디스크가 순서대로(작은 디스크가 맨 위) 놓여있을 때 다음 규칙에 따라 한 기둥에서 목표하는 기둥으로 모든 디스크를 옮기는 것이 목표다.

1. 한번에 한 디스크만 이동할 수 있다.
2. 작은 디스크 위에 큰 디스크를 놓을 수 없다.

관련 내용이나 그림은 인터넷 등을 찾아보라.

`n`개와 `a`, `b`, `c` 기둥이 있을 때 `a`에서 `c`로 디스크를 모두 이동(이때 `b`를 임시 저장 기둥으로 사용한다) 과정은 다음과 같은 재귀 알고리즘을 정의할 수 있다.

1. `n-1`개의 디스크를 `a`에서 `b`로 옮겨라. 이때 `c`를 임시 저장 기둥으로 사용할 수 있다.  
2. `a`에 있는 디스크(가장 큰 디스크일 것임)를 `c`로 옮겨라.
3. `b`에 있는 `n-1`개의 디스크(순서대로 놓여있을 것임)를 `b`에서 `c`로 옮긴다(이때 `a`를 임시 저장 기둥으로 사용할 수 있다).

여기서 1과 3에서 `n-1`개의 디스크를 이동하는 과정에 이 알고리즘을 재귀 적용할 수 있다. 

이 연습문제에서는 `hanoi`라는 함수를 만들자.

```haskell
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
```

여기서 `type`는 어떤 타입에 별명을 부여하는 하스켈 선언이다. 

디스크가 2개인 경우 결과는 다음과 같다.

```haskell
hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
```

이를 해석하면 다음과 같다.

1. 총 2개의 디스크가 있다.
2. `a`는 디스크가 현재 위치한 기둥의 이름이다.
3. `b`는 디스크를 옮길 대상 기둥의 이름이다.
4. `c`는 중간 저장소로 사용할 기둥의 이름이다.

결과 리스트를 해석하면 다음과 같다.

1. `a`에 있는 (작은) 디스크를 `c` 기둥으로 옮긴다.
2. `a`에 있는 (큰) 디스크를 `b` 기둥으로 옮긴다.
3. `c`에 있는 (작은) 디스크를 `b` 기둥으로 옮긴다.

이런 이동을 거치고 나면 `a` 기둥에 있던 2개의 디스크가 모두 `b` 기둥으로 순서를 유지하면서 이동한다.

### (선택문제) 하노이 탑 변형 

기둥이 3개가 아니라 4개라면 어떤 일이 벌어질까? 이때 가능한 최소한의 횟수로 디스크를 목표하는 탑으로 이동시키는 알고리즘을 생각하고 그 알고리즘을 `hanoi4`라는 함수로 구현하라.

```haskell
type Peg = String
type Move = (Peg, Peg)
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
```

기둥이 1개 늘어나면 훨씬 더 적은 회수로 이동이 가능하다. 예를 들어 15 디스크를 이동시키려면 3개의 기둥에서는 2^15 - 1 = 32767 번의 이동이 필요하지만, 4개의 기둥을 사용하면 129번만에 가능하다. Addison-Wesley사에서 1994년 나온 Graham, Knuth, Patashnik저, *Concrete Mathematics(2판)*의 연습문제 1.17을 보라.