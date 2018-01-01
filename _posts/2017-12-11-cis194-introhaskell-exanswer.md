---
layout:     post
title:      "[하스켈 기초][CIS194] 1강 하스켈 소개 - 연습문제 풀이"
date:       2017-12-11 00:46:00
summary:    "CIS194 1강 연습문제 풀이"
categories: Haskell CIS194 Introduction Exercise Solution
---

1강 연습 문제를 풀어보자.

## 카드 검증

### 연습문제 1

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

#### 해법

크게는 두 가지 계통의 방법이 있을 것이다. 

1. 편하지만 느린 방법: 정수를 일단 문자열로 만들 수만 있다면(대부분의 언어는 이런 변환 함수를 제공한다) 문자열을 다시 수로 변환하면 된다.
예를 들어 `1234`를 `"1234"`로 변환하면 하스켈에서 이는 `['1','2','3','4']`라는 리스트와 같으므로 다시 이 리스트의 각 숫를 그에 해당하는 정수 값으로 바꾸면 된다.

2. 정수를 사용하는 방법: 어떤 수 `x`를 10으로 나눈 나머지와 10으로 나눈 몫의 2가지 부분으로 나눌 수 있다. 예를 들어 `1024`를 `(102,4)`로 나누는 함수 `remdiv`가 있다면, `102`에 다시 `remdiv`를 적용하면 `(10,2)`을 얻을 수 있고, `10`에 한번 더 `remdiv`를 적용하면 `(1,0)`을 얻을 수 있다. 여기서`1`은 10보다 작으므로 더이상 `remdiv`를 적용할 필요가 없다. 이제 10보다 작아진 몫을 맨 앞에 쓰고, 지금까지 구했던 나머지를 역순으로 쓰면 `1`,`0`,`2`,`4`를 얻을 수 있다. 

#### 구현 

첫번째 방법을 구현하려면 정수를 문자열로 바꾸는 함수를 알아야 한다. 구글링을 해보면 스택오버플로우에서 `show`가 수를 문자열로, `read`가 문자열을 수로 바꿔준다고 한다. 어? 그럼 혹시 `Char`도 `read`로 수로 바꿀 수 있지 않을까? 한번 ghci에서 실험해보자.

```haskell
Prelude> read '1' :: Integer

<interactive>:4:6: error:
    ? Couldn't match type ‘Char’ with ‘[Char]’
      Expected type: String
        Actual type: Char
    ? In the first argument of ‘read’, namely ‘'1'’
      In the expression: read '1' :: Integer
      In an equation for ‘it’: it = read '1' :: Integer
Prelude> read ['1'] :: Integer
1
```

타입 에러가 난다. 뭐.. `['1']` 처럼 `[]`로 문자를 둘러싸면 해결되니 일단 이걸로 만족하자. 필요하면 더 구글링해서 `Char`의 유니 코드 값을 구해서 48(`'0'`의 코드값)을 빼면 되겠지만 오늘은 이걸로 만족하자.

(2018년 1월 1일: `Data.Char`에 보면 `digitToInt`라는 연산이 있다. 16진수의 문자(`0`-`9`,`a`-`f`,`A`-`F`)를 그에 상응하는 수로 바꿔준다.)

문자열을 수 리스트로 바꿔주는 함수를 정의해야 한다. `toDigitList`라는 함수를 하나 만들자. 재귀적으로 리스트의 첫번째 원소를 `read`를 사용해 바꿔주면 될 것이다. 리스트 패턴 매치 방법을 다시 기억해보자.

`toDigitsRev`는 물론 `toDigits`를 하고 그 결과를 뒤집으면 된다. 일단 오늘은 그 정도 수준에서 만족하자.

아직은 본격적인 테스트 프레임워크 등을 사용할 수는 없으므로 `==`를 사용한 비교로 테스트를 수행한다.

```haskell
-- ex1-1.hs
import Control.Exception  -- assert를 사용하기 위해 임포트

toDigitList :: [Char] -> [Integer]
toDigitList [] = []
toDigitList (x:xs) = (read [x] ::Integer) : toDigitList xs 

toDigits :: Integer -> [Integer]
toDigits n 
  | n <= 0 = []
  | otherwise = toDigitList (show n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n) 

-- test 코드
test v1 v2 s = assert (v1==v2) s

-- 최상위 수준에 날 식이 들어가면 `parse error: naked expression at top level`이라는 오류가 나옴 
-- 리스트를 만들고 리스트를 출력하는 함수를 만들어서 `doTest`라는 이름을 붙이자.

testValue = [
  test (toDigitList ['0','1']) [0,1] "toDigitList[0,1] success",
  test (toDigitList []) [] "toDigitList[] success",
  test (toDigits 1) [1] "toDigits 1 success",
  test (toDigits 101023) [1,0,1,0,2,3] "toDigits 101023 success",
  test (toDigits 9876543210987654321) [9,8,7,6,5,4,3,2,1,0,9,8,7,6,5,4,3,2,1] "toDigits 987654321987654321 success",
  test (toDigitsRev 101023) [3,2,0,1,0,1] "toDigitsRev 101023 success",
  test (toDigitsRev2 101023) [3,2,0,1,0,1] "toDigitsRev2 101023 success"
            ]

doTest = print testValue
```

### 연습문제 2

리스트의 맨 끝을 기준으로 매 2번째마다 있는 수를 2배하는 함수를 만들라.

- `doubleEveryOther :: [Integer] -> [Integer]`

예제:

```haskell
doubleEveryOther [8,7,6,5] == [16,7,12,5]
doubleEveryOther [1,2,3] == [1,4,3]
```

#### 해법

오른쪽부터 매 2번째 원소를 2배 해야 한다. 왼쪽부터 따져서 매 2번째 원소를 2배하기는 쉽다. 일단 그것부터 생각해보자. 재귀적 해법을 찾을 수 있을까?

1. 원소가 없는 리스트는 그냥 `[]`를 반환한다.
2. 원소가 1개는 리스트는 그 원소를 반환한다.
3. 원소가 2개 이상 있는 리스트는 1번째 원소는 그대로, 2번째 원소는 2배한 다음에, 지금 이야기하는 해법을 나머지 리스트에 적용해 서로 이어붙인다.

이 해법이 제대로 작동함을 어떻게 증명할 수 있을까? 수학적 귀납법을 쓰면 된다. 여기선 원소 개수에 대해 귀납법을 쓰면 될 것이다. 연습문제로 남겨둔다.

일단 이렇게 매 2번째 원소를 2배하는 알고리즘이 있다면 리스트를 뒤집고 이 알고리즘을 적용한 다음에 다시 결과 리스트를 뒤집으면 쉽게(하지만 컴퓨터는 고생한다) 목표를 달성할 수 있다.

리스트 길이를 한번에 알 수 있다면 길이를 가지고 짝/홀수에 따라 알고리즘을 구성할 수 있지만 길이를 측정하기 위해 1번, 알고리즘이 또 1번, 이렇게 최소 두번은 어쩔 수 없이 리스트 원소를 모두 뒤져야 한다. 이렇게 구현하는 것은 나중에 심심할때 한번 해보기로 한다.

#### 구현 

딱히 힘든 부분이 없다.

```haskell
-- ex1-2.hs
import Control.Exception  -- assert를 사용하기 위해 임포트

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft (x:[]) = x:[]
doubleEveryOtherFromLeft (x1:x2:xs) = x1:(x2*2):(doubleEveryOtherFromLeft xs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherFromLeft (reverse xs))

-- test 코드
test v1 v2 s = assert (v1==v2) (s)

deol = doubleEveryOtherFromLeft
deor = doubleEveryOther

testValue = [
    test (deol [1,2,3,4]) [1,4,3,8] "deol [1,2,3,4]",
    test (deor [1,2,3,4]) [2,2,6,4] "deor [1,2,3,4]"
            ]

doTest = print testValue
```

### 연습문제 3

2자리나 1자리 수가 섞인 리스트의 모든 숫자를 더하는 함수를 만들라.

- `sumDigits :: [Integer] -> Integer`

예제:

```haskell
sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
```

#### 해법

다양한 방법이 있을 것이다. 

먼저 생각할 수 있는 방법은 리스트 원소 중에 두자리 수를 두개의 한자리 수로 바꿔주는 함수를 만들고, 그렇게 변환한 리스트에 대해 리스트트의 모든 원소의 합계를 구하는 함수를 적용하는 방법이다.

다음 방법으로는 두자리 수는 각 자리의 숫자의 합계를 구해주고(예: `17 => 1+7 => 8`) 한자리 수는 그 수를 그대로 반환하는 함수를 사용하는 방법이 있다. 리스트의 모든 원소에 대해 이 함수를 적용해가면서 모든 원소의 합계를 구하면 쉽게 `sumDigits`를 만들 수 있다. 

#### 구현

첫번째 방식이나 두반째 방식에서 모두 두자리 수의 값은 최대 18(0~9의 아라비아 숫자에 해당하는 수를 2배한 것이므로)이다. 따라서 함수의 입력 값이 제한되므로 10을 넘는지만 가드를 사용해 판단하면 쉽게 함수를 적용할 수 있다.

첫번째 방식을 생각해 보자. 두자리 수를 두개의 한자리 수, 한자리 수를 그자신으로 바꿔주는 함수의 타입은 뭐가 되야 할까? `Integer`가 (유일한) 파라미터의 타입이라는 사실은 쉽게 알 수 있다. 반환 타입은 `[Integer]`가 적당할 것이다. 한자리 수와 두자리 수가 반환하는 수의 개수가 2개와 1개로 다른데 리스트로 처리하면 이를 자연스럽게 처리할 수 있다. 입력 검사 등은 생략한다.

```haskell
numToOneDigitValues :: Integer -> [Integer]
numToOneDigitValues n 
  | n >= 10 = [1,n-10] -- n이 0부터 18 사이라는 사실을 활용
  | otherwise = [n]
```

이 함수를 가지고 리스트의 모든 원소를 변환하자. 이를 위해 `[Integer]` 리스트의 모든 원소에 대해 `Integer -> [Integer]`인 함수를 적용한 결과를 리스트로 모아 돌려주는 함수를 만들 수 있다.

```haskell
transform :: [Integer] -> (Integer->[Integer]) -> [[Integer]] 
transform [] f = []
transform x::xs f = f(x) : transform x xs 
```

`transform`과 `numToOneDigitValues`가 있으면 정수 리스트를 받아서 정수 리스트의 리스트를 만들 수 있다. 그 리스트의 각 리스트에 들어있는 모든 원소를 펼쳐주는 함수를 만들자. 각 원소가 리스트이므로 리스트 뒤에 리스트를 이어붙이는 함수가 필요하다.

```haskell
append :: [Integer] -> [Integer] -> [Integer]
append [] ys = ys 
append (x:xs) ys = x : append xs ys
```

이렇게 리스트에 리스트를 이어붙이는 함수가 있으면 리스트안에 리스트가 들어있을 때 모든 리스트의 원소를 펼친 단일 리스트로 만드는 함수를 만들 수 있다.

```haskell
flatten :: [[Integer]] -> [Integer]
flatten [] = []
flatten xs::xss = append xs (flatten xss)
```

이제 펼친 리스트가 있으면 리스트 안의 모든 원소를 더하는 `sumList`를 만들 수 있다.

```haskell
sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs
```

이 모든 함수가 준비되면 실제 구현은 단순하다.

```haskell
sumDigits :: [Integer] -> Integer
sumDigits xs = sumList (flatten (transform xs numToOneDigitValues))
```

`flatten`, `append`, `transform`, `sumList` 와 같은 함수는 리스트를 처리할 때 자주 보게 될 함수다. 나중에 (아마도) 강의에서 이에 대해 다뤄주리라 생각한다.

방금까지 구현한 내용을 한 파일에 저장하면 다음과 같다.

```haskell
-- ex1-2.hs
import Control.Exception  -- assert를 사용하기 위해 임포트


numToOneDigitValues :: Integer -> [Integer]
numToOneDigitValues n 
  | n >= 10 = [1,n-10] -- n이 0부터 18 사이라는 사실을 활용
  | otherwise = [n]

transform :: [Integer] -> (Integer->[Integer]) -> [[Integer]] 
transform [] f = []
transform (x:xs) f = f x  : transform xs f

append :: [Integer] -> [Integer] -> [Integer]
append [] ys = ys 
append (x:xs) ys = x : append xs ys

flatten :: [[Integer]] -> [Integer]
flatten [] = []
flatten (xs:xss) = append xs (flatten xss)

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs

sumDigits :: [Integer] -> Integer
sumDigits xs = sumList (flatten (transform xs numToOneDigitValues))

-- test 코드
test v1 v2 s = assert (v1==v2) (s)

ntodv = numToOneDigitValues

testValue = [
    test (ntodv 1) [1] "ntodv 1",
    test (ntodv 18) [1,8] "ntodv 1",
    test (transform [1,18] ntodv) [[1],[1,8]] "transform [1,18]",
    test (transform [1] ntodv) [[1]] "transform [1]",
    test (transform [17] ntodv) [[1,7]] "transform [17]",
    test (append [] []) [] "append [] []",
    test (append [1] []) [1] "append [1] []",
    test (append [] [2]) [2] "append [] [2]",
    test (append [1,2] []) [1,2] "append [1,2] []",
    test (append [] [1,2]) [1,2] "append [] [1,2]",
    test (append [1,2] [3,4]) [1,2,3,4] "append [1,2] [3,4]",
    test (append [1,2,3,4] [5,6,7,8]) [1,2,3,4,5,6,7,8] "append [1,2,3,4] [5,6,7,8]",
    test (flatten [[]]) [] "flatten [[]]",
    test (flatten [[1]]) [1] "flatten [[1]]",
    test (flatten [[1],[2,3],[4,5,6]]) [1,2,3,4,5,6] "flatten [[1],[2,3],[4,5,6]]",
    test (flatten [[1,2,3],[2,3,4],[3,4,5]]) [1,2,3,2,3,4,3,4,5] "flatten [[1,2,3],[2,3,4],[3,4,5]]",
    test (sumList []) 0 "sumList []",
    test (sumList [1]) 1 "sumList [1]",
    test (sumList [1..10]) 55 "sumList [1..10]",
    test (sumDigits [12,3,4,5,6,7,8,9,10]) 46 "sumDigits [12,3,4,...,10]",
    test (sumDigits [1,2,3,4,5,6,7,8,9,10]) 46 "sumDigits [12,3,4,...,10]"
            ]

doTest = print testValue
```

### 연습문제 4

카드를 검증하는 함수를 만들라.

- `validate :: Integer -> Bool`

이 함수는 앞에서 정의한 함수들을 활용해야 한다.

예제:

```haskell
validate 4012888888881881 = True
validate 4012888888881882 = False
```

#### 해법

카드 번호를 `toDigits`로 수의 리스트로 만들고, `doubleEveryOther`로 오른쪽부터 매 2번째 있는 수를 2배한 다음에, `sumDigits`로 모든 숫자를 더해서 10으로 나눠보면 된다.

#### 구현 

더이상의 설명은 생략한다.

```haskell
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) == 0
```

## 하노이 탑

하노이 탑은 워낙 유명한 문제라 전산 전공이라면 한번쯤은 플어봤을 것이다. 별로 어렵지 않다. 알고리즘 설명을 그대로 코드로 옮기자.

문제에서는 `hanoi n "a" "c" "b"`에 대한 알고리즘을 설명했다. 이를 `hanoi n src tgt tmp`를 호출한 것으로 생각하고 다시 적어보면 다음과 같다.

1. `n-1`개의 디스크를 `src`에서 `tmp`로 옮겨라. 이때 `tgt`를 임시 저장 기둥으로 사용한다.
2. `src`에 있는 디스크(가장 큰 디스크일 것임)를 `tgt`로 옮겨라.
3. `tmp`에 있는 `n-1`개의 디스크(1에서 옮겼음. 순서대로 놓여있을 것임)를 `tmp`에서 `tgt`로 옮긴다. 이때 `src`를 임시 저장 기둥으로 사용할 수 있다.

이를 함수 호출 표현으로 바꾸면 다음과 같다.

1. 재귀적으로 `hanoi (n-1) src tmp tgt`를 호출한다.
2. 1의 결과 리스트 뒤에 `(src, tgt)`라는 무브를 추가한다.
3. 재귀적으로 `hanoi (n-1) tmp tgt src`를 호출한 결과 리스트를 2의 결과 리스트에 추가한다.

한편, `n`이 0이면 디스크를 옮길 필요가 없다.

이제 이를 하스켈 코드로 표현하기만 하면 된다. 하스켈에서는 항상 타입을 기준으로 가능한 연산이 뭔지 고민하는 습관을 들여야 한다.

1. `hanoi (n-1) src tmp tgt`는 `[Move]` 타입이다.
2. `(src, tgt)`라는 무브는 `Move` 타입이다.
3. `hanoi (n-1) tmp tgt src`는 `[Move]` 타입이다.

1,2,3을 연결하려면 `[Move]` 타입의 리스트 뒤에 `Move` 타입인 원소를 하나 추가해서 만들어진 리스트 뒤에 `[Move]` 타입의 리스트를 덧붙여야 한다. 리스트 맨 끝에 원소를 추가하는 연산은 리스트 맨 끝에 원소가 하나뿐인 리스트를 추가하는 연산과 같다. 연습문제 3에서 만든 `append` 함수는 `[Integer]` 리스트 뒤에 `[Integer]` 리스트를 추가해 주는 것이었다. 이 함수를 그대로 가져와서 타입만 바꾸면 여기서 써먹을 수 있는 `append`가 된다.

```haskell
type Peg = String
type Move = (Peg, Peg)

append :: [Move] -> [Move] -> [Move]
append [] ys = ys 
append (x:xs) ys = x : append xs ys
```

이제 앞에서 설명한 3가지 단계와 `n`이 0일때의 종료 조건을 하스켈 코드로 만들자.

```haskell
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 src tgt tmp = []
hanoi n src tgt tmp = append (append (hanoi (n-1) src tmp tgt) [(src,tgt)]) (hanoi (n-1) tmp tgt src) 
```

하지만 함수 호출에 괄호가 많아 보기가 좋지 않다. `mod`를 백틱을 사용해 중위 호출한 것을 본따 다음과 같이 백틱을 활용해 `append` 호출을 중위 호출로 바꾸는 편이 낫다.

```
hanoi2 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi2 0 src tgt tmp = []
hanoi2 n src tgt tmp = (hanoi2 (n-1) src tmp tgt) `append` [(src,tgt)]  `append` (hanoi2 (n-1) tmp tgt src) 
```

백틱을 사용해 중위 연산자처럼 쓸때는 함수 적용보다 우선순위가 낮다는 점을 활용해 괄호를 아예 없애면 다음과 같이 쓸 수도 있다.

```
hanoi3 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi3 0 src tgt tmp = []
hanoi3 n src tgt tmp = hanoi3 (n-1) src tmp tgt `append` [(src,tgt)]  `append` hanoi3 (n-1) tmp tgt src
```

## 하노이탑 변형(탑이 4개)

아마도 최적의 방법은 다음과 같을 것이다.

`hanoiFour n src tgt tmp1 tmp2`를 호출한 경우:

1. 절반의 디스크를 `src`에서 `tmp1`으로 옮겨라. 이때 `tgt`,`tmp2`를 임시 저장 기둥으로 사용한다.
2. 1에서 옮기고 남은 디스크들을 `src`에서 `tgt`로 옮겨라. 이때는 `tmp1`에 작은 디스크들이 있기 때문에 `tmp2`만을 임시 저장기둥으로 사용해야 한다. 
3. 절반의 디스크를 `tmp1`에서 `tgt`로 옮겨라. 이때 `src`,`tmp2`를 임시 저장 기둥으로 사용한다.

1에서 몇개의 디스크를 옮기느냐에 따라 알고리즘의 복잡도가 달라질 것 같다. 이 알고리즘이 최적인지는 옮기는 개수를 정하는 규칙에 따라 전체 옮기는 횟수가 어떻게 될지 점화식 등으로 표현한 다음에 최소값을 구하는 방식으로 증명할 수 있을 것 같다.

구현? 내가 페르마는 아니지만 여백이 모잘라서... 