---
layout:     post
title:      "[하스켈 기초][CIS194] 재귀 연습문제 풀이"
date:       2018-01-08 20:45:00
summary:    "CIS194 3강 재귀 관련 연습문제 풀이"
categories: Haskell CIS194 Recursion Exercise Solution
---

[CIS194 3강 재귀, 패턴, 다형성, 프렐류드](http://enshahar.com/haskell/cis194/recursion/pattern/prelude/2018/01/06/cis194-recursion/)에 있는 연습문제 풀이입니다.

## 연습문제1 - 홉스코치(Hopscotch)

1번 문제는 여러 방법으로 풀 수 있겠지만, 다음과 같은 단계로 나눠서 풀면 될 것 같다.

1. 0부터 리스트 길이-1까지 1씩 증가하는 수열(series) 또는 범위(range)를 만든다.
2. 입력으로 들어온 리스트 원소를 n개씩 건너뛰면서 원소를 취하는 `skipN n list` 함수를 만든다.
3. 1번의 리스트에 대해 `\n -> skipN n list`를 `map`한다.

### `0`부터 `n-1` 까지의 수열 또는 범위 만들기

물론 재귀로 다음과 같이 쉽게 풀 수 있다.

```haskell
rangeSub :: Int -> [Int]
rangeSub 0 = []
rangeSub n = (n-1) : rangeSub (n-1)

range :: Int -> [Int]
range 0 = []
range n = reverse(rangeSub n)
```

나중에 배우겠지만 `rangeSub`를 `range` 안에서 정의하면 함수 네임스페이스를 더럽히지 않아 좋다. 이럴때 `let`이나 `where`를 사용한다.

```haskell
range :: Int -> [Int]
range n = let 
             rangeSub 0 = []
             rangeSub n = (n-1) : rangeSub (n-1)
          in
             reverse(rangeSub n)
```

`reverse`를 써서 뒤집는게 맘에 안든다면 0부터 차례대로 리스트를 만드는 내부 함수를 만들 수도 있다. 이때 정적 변수 영역(lexical scoping)을 활용할 수 있다.

```haskell
range :: Int -> [Int]
range 0 = []
range n = let
              rangeSub k | k == n = []
              rangeSub k = k : rangeSub (k+1)
          in
              rangeSub 0
```

흔히 쓰이는 패턴이므로 하스켈이 기본 제공하는 시퀀스 연산자가 있다. 리스트를 표현하는 `[]`안에서 `..`를 쓰면 된다. 다음 예제를 살펴보자. `ghci`에서는 리스트를 화면에 찍기 위해 리스트 원소 값들을 생성해 내므로 무한 리스트의 경우 계속 원소를 화면에 찍는다. *Ctrl-C*로 인터럽트를 걸어 출력을 중단시킬 수 있다. 문자열이 문자의 리스트라는 사실을 `['a'..'e']`에서도 확인할 수 있다.

```haskell
['a'..'e'] = "abcde"
[1,3..10] = [1,3,5,7,9]
[1,11..] = [1,11,21,31,...]
[1..] = [1,2,3,4,...]
```

실제로는 이는 다음을 간편하게 쓰도록 허락한 것이다. [하스켈 보고서(명세)](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-400003.10)의 3.10절을 보라.

```haskell
[ e1.. ]      = enumFrom e1
[ e1,e2.. ]   = enumFromThen e1 e2
[ e1..e3 ]    = enumFromTo e1 e3
[ e1,e2..e3 ] = enumFromThenTo e1 e2 e3
```

여기서 `From`에 해당하는 값은 시작 값, `To`에 해당하는 값은 끝값(끝값과 같은 값도 리스트에 포함됨), `Then`에 해당하는 값은 시작값 다음의 값으로, 시작값부터 어떤 간격으로 원소들을 만들어낼지 결정한다. `To`가 없으면 무한 리스트가 생긴다는 점에 유의하라. 다만 하스켈은 지연계산(lazy evaluation) 언어라서 무한 리스트를 무한정 만들어내지 않고 실제 필요한 값만 나중에 계산해낸다.

리스트 컴프리핸션(list comprehension)으로도 이런 리스트를 만들 수 있다. 나중에 배우게 될 것이고 구글링해 나도 배워야 하기 때문에 여기 쓰지는 않겠다.

숙제 문제 해결에는 `[0..(length(xs)-1)]`정도가 적당할 것 같다. `xs`는 함수가 받은 리스트에 붙인 이름이다.

### 리스트 원소를 n개씩 건너뛰면서 원소를 취하는 함수

프렐류드에는 리스트의 처음 `n`개를 건너뛰는 `drop` 함수가 있다. `drop 3 [1,2,3,4,5] = [4,5]`이다. 이를 재귀적으로 활용하면 `n`개씩 건너뛰면서 원소를 취하는 함수를 만들 수 있다.

```haskell
skipN :: Int -> [a] -> [a]
skipN n xs = case (skip xs) of 
                 [] -> []
                 (y : ys) -> y : skipN n ys
```

### 0부터 (길이-1)까지의 정수 리스트에 `skipN`을 `map`하기

이제 `..`를 사용해 정수 리스트를 만들고 그 리스트에 `skipN`을 `map`으로 적용하면서 인자로 받은 리스트를 전달하면 된다.

```haskell
skips :: [a] -> [[a]]
skips xs = map (\x -> skipN x xs) [0..(length xs - 1)]
```

### 전체 코드

전체 코드는 다음과 같다. 골프 스코어는 신경쓰지 않는다. 효율이 O(N^2)으로 약간 떨어지는 것과 무한 리스트에 대해 사용할 수 없다는 점도 약간 꺼림찍하지만 일단 넘어가자.

```haskell
-- Ex1.hs
skipN :: Int -> [a] -> [a]
skipN n xs = case (drop n xs) of
                 [] -> []
                 (y:ys) -> y : skipN n ys

skips :: [a] -> [[a]]
skips xs = map (\x -> skipN x xs) [0..(length xs - 1)]
```

## 연습문제2 - 지역 최댓값

일단 간단하게는 3개씩 묶어서 슬라이딩 윈도우(sliding window)를 만들어서 비교해보면 될 것 같다. 즉, `[2,9,5,6,1]`을 `[[2,9,5],[9,5,6],[5,6,1]]`로 일단 만들고, 원소가 3개인 리스트의 리스트에서 가운데 값이 양쪽 값보다 더 큰 리스트들만 남긴 다음에, 다시 `map`을 써서 가운데 값만 남기면 될 것이다. 슬라이딩 윈도우를 만드는 함수를 재귀로 정의하긴 어렵지 않다. 독자들에게 숙제로 남겨둔다.

한편 여기서는 3개씩 원소를 묶으면 되므로 `zip3`를 써서 리스트를 묶는게 더 편해 보이기도 한다. `zip3`는 3개의 리스트를 묶어서 3-튜플의 리스트로 만들어 주는 함수다. 길이가 맞지 않을 때는 가장 짧은 리스트가 전체 길이를 결정한다.

```haskell
zip3 [1,2,3] [4,5,6] [7,8,9] = [(1,4,7),(2,5,8),(3,6,9)]
zip3 [] [4,5] [6] = []
```

다음과 같이 3-튜플의 리스트를 만들어내는 `mk3Tuples`를 정의하자.

```haskell
mk3Tuples :: [Integer] -> [(Integer,Integer,Integer)]
mk3Tuples xs = zip3 xs (drop 1 xs) (drop 2 xs)
```

이제 리스트를 `mk3Tuples`로 3-튜플로 만들고, `filter`를 써서 가운데 값이 더 큰 3-튜플만 남기자. 3-튜플에 대해 패턴매칭을 사용하면 쉽게 지역 최댓값인지 판단하는 술어함수를 만들 수 있다. 람다로 정의하고 따로 타입은 표시하지 않는다.

```haskell
isLocalMaxima = \(x,y,z) -> x < y && y > z
```

이 함수가 있으면 다음과 같이 지역 최댓값 3-튜플만 남기는 함수를 만들 수 있다.

```haskell
findLocalMaximaTriples xs = filter isLocalMaxima (mk3Tuples xs)
```

다시 이 결과에서 가운데 원소만 남기면 된다. `map`에 넘길 함수르 3-튜플에서 2번째 값을 얻어오는 `tuple3get2nd`를 만들자.

```haskell
tuple3get2nd = \(_,y,_) -> y
```

이제 전체 결과는 `findLocalMaximaTriples`의 결과에 `tuple3get2nd`를 `map`한 것이다.

```haskell
localMaxima xs = map tuple3get2nd (findLocalMaximaTriples xs)
```

### 전체 코드

모든 함수들을 한 `map` 호출에 다 넣을 수도 있지만 그리 이쁘지 않다. 그냥 각각의 정의를 따로 두되, `findLocalMaximaTriples`는 따로 쓸모가 없으므로 최종 `localMaxima` 정의에 넣어버리자.

```haskell
-- Ex2.hs
isLocalMaxima = \(x,y,z) -> x < y && y > z
tuple3get2nd = \(_,y,_) -> y

mk3Tuples :: [Integer] -> [(Integer,Integer,Integer)]
mk3Tuples xs = zip3 xs (drop 1 xs) (drop 2 xs)

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map tuple3get2nd (filter isLocalMaxima (mk3Tuples xs))
```

## 연습문제 3 - 히스토그램

이 문제는 도수분포를 계산하는 것과 그래프를 출력하는 것으로 나눠 생각할 수 있다. 

### 도수분포 계산하기

일반적인 해법은 다음과 같다.

1. 빈 맵(사전,딕셔너리 등등으로 부르기도 함)을 하나 만든다.
2. 리스트의 원소를 스캔하면서 다음과 같은 연산을 수행한다. 현재 스캔중인 원소를 `x`라 하자.
    1. 맵 안에 `x`를 키로 하는 원소가 없다면 `x`를 키로 1이라는 빈도를 맵에 넣는다.
    2. 맵 안에 `x`를 키로 하는 원소가 있다면 그 값(=빈도수)에 1을 더한 새 빈도를 맵에 넣는다.

명령형 언어에서는 맵을 갱신할 것이고, 함수형 언어에서는 기존 맵에 새 원소를 덧붙인 새 맵을 만들수 있을 것이다. 하스켈 프렐류드를 찾아봐도 딱히 맵이나 딕셔너리 데이터 타입이 없다. 필요하면 [`Data.Map`](https://hackage.haskell.org/package/containers-0.4.2.0/docs/Data-Map.html)을 사용할 수는 있겠지만(사용법을 익혀 두는게 편할지도 ^^)... 이 부분은 문서를 찾아서 구현하면 되겠지만 딱히 흥미로운 것이 없어서 일단 숙제로 남겨둔다.


#### 간단한 맵 구현

바퀴를 다시 만들면 안되지만, 정적 변수 영역 지정을 활용하면 함수를 마치 데이터 구조처럼 다룰 수 있음을 보여주기 위해 맵을 함수로만 구현해 보자. 우리는 정수를 받아서 빈도수를 돌려주는 맵이 필요하다. 따라서 우리가 사용할 맵의 타입은 `Int -> Int`이다. 맨 처음 이 맵은 모든 수에 대해 0을 돌려준다.

```haskell
initialMap :: Int -> Int
initialMap x = 0
```

맵을 검색하는 것은 맵에 원하는 정수를 넣는 것으로 해결된다. 맵에 새 키/값 쌍을 추가하는 것은 기존 맵, 새 키, 새 값을 지정하는 것으로 가능하다. 

```haskell
addMap map key value = \k -> if k==key then value else (map k)
```

이때 `addMap`이 반환하는 것이 `Int -> Int` 타입의 람다라는 점에 유의하라. 이 람다는 인자로 받은 정수가 `key`에 해당하는 경우 지정된 `value`를 반환하므로 `map`에 `key`에 해당하는 값이 들어있었더라도 기존 맵을 덮어쓰는 것과 겉으로 볼 때는 같은 역할을 한다. 한편 람다가 `key`와 다른 값을 인자로 받으면 기존 `map`에게 다시 그에 해당하는 값을 물어보므로 `key`가 아닌 모든 값에 대해서는 기존 `map`과 같은 결과를 내놓는다.

이 맵에서 원소를 제거할 수는 없다. 물론 `Maybe`등을 사용해 맵 안에 값이 들어있지 않음을 표현하게 바꾸면 원소 제거도 방금 본 `addMap`과 사실상 다르지 않은 방식으로 구현 가능하다.

이제 리스트를 방문하면서 `initialMap`에 각 값의 빈도수를 쌓아가면 된다. 최종적으로 원하는 것은 `Int -> Int` 타입의 맵이다. 즉, `[Int]`의 모든 원소에 대해 어떤 연산을 수행하되, 최종 결과는 리스트의 모든 정보를 축약한 맵 타입(우리는 `Int -> Int`를 맵 타입으로 정했다)이다. 이렇게 리스트의 원소들을 어떤 다른 타입의 값으로 정리하면서 항상 같은 연산을 적용할 수 있는 경우 `reduce`나 `fold`등의 축약/접기 연산을 사용할 수 있다. 하스켈에는 `foldl`, `foldr` 등의 연산이 있다. 하스켈 문서를 보면 다음과 같이 리스트에 대한 `foldl`과 `foldr`의 결과를 설명한다.

```haskell
foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
```

우리의 경우 둘 중 어떤 것을 활용해도 관계 없을 것이다. 그냥 `foldl`을 사용하자. `foldl`의 타입은 `Foldable t => (b -> a -> b) -> b -> t a -> b` 이다. (여기서 `Foldable t`는 그냥 축약할 수 있는 어떤 데이터 타입 `t`라고 생각하면 된다. `=>` 다음이 함수의 타입이라고 생각하면 된다. 리스트도 `Foldable`이므로 리스트를 `t`로 대치하면 리스트의 경우 `foldl`의 타입은 다음과 같다.

```haskell
foldl :: (b -> a -> b) -> b -> List a -> b

-- 또는

foldl :: (b -> a -> b) -> b -> [a] -> b
```

우리의 경우 리스트 원소의 타입은 `Int`이고, 축약 결과인 맵의 타입인 `b`는 `Int -> Int`다. 초깃값으로는 모든 수에 대해 0을 반환하는 `initialMap`을 넘기면 된다. 따라서 다음과 같은 형태가 될 것이다.

```haskell
calcDist xs = foldl (어떤연산) initialMap xs
```

이제 `(어떤연산)`에 대해 생각해보자. `(어떤연산)`의 타입은 `b -> Int -> b`이고, `b`는 `Int -> Int`여야 한다. 이 연산은 리스트의 원소를 두번째 인자로 받고, 첫번째 인자로는 지금까지 리스트의 정보가 축약된 맵(`Int -> Int` 함수)을 인자로 받는다. 이 연산이 돌려주는 결과는 리스트의 원소에 해당하는 빈도를 1 증가시킨 새로운 맵이다. 이를 람다로 써보자.

```haskell
어떤연산 = \map -> \x -> addMap map x ((map x) + 1)
```

이를 `foldl`에 넣으면 다음과 같이 정리된다.

```haskell
calcDist xs = foldl (\map -> \x -> addMap map x ((map x) + 1)) initialMap xs
```

한편 이 `calcDist`가 돌려주는 값은 `Int -> Int` 타입인 함수다. 따라서 그 안에 들어있는 빈도수를 뽑아보려면 다음과 같이 0부터 9까지의 리스트에 대해 `calcDist`를 `map`해야 한다.

```haskell
E:\blog\example\haskell\cis194\03_recursion
λ ghci
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from C:\Users\hyunsok\AppData\Roaming\ghc\ghci.conf
Prelude> initialMap x = 0
Prelude> addMap map key value = \k -> if k==key then value else (map k)
Prelude> calcDist xs = foldl (\map -> \x -> addMap map x ((map x) + 1)) initialMap xs
Prelude> distMap = calcDist [1,2,3,4,1,5,1,4,6,7,7]
Prelude> map distMap [0..9]
[0,3,1,1,2,1,1,2,0,0]
Prelude>
```

실제로는 이 리스트를 가지고 필요한 작업을 충분히 수행할 수 있다. `calcDist` 자체를 리스트를 반환하는 함수로 바꿔보자.

```haskell
calcDist xs = map (foldl (\map -> \x -> addMap map x ((map x) + 1)) initialMap xs) [0..9]
```


#### 느리지만 간단한 방법(참고)


느리지만 그냥 편한 방법으로는 입력 리스트를 정렬한 다음에 앞에서부터 같은 숫자끼리 그룹화 시켜서 각 그룹의 길이를 재는 방법이 있다. 다행히 하스켈은 `sort`와 `group`이라는 함수를 제공한다. 

```haskell
calcDist xs = map (\x -> (head(x), length(x))) (group (sort xs))
```

이경우 빈도가 0인 원소는 최종 리스트에 들어있지 않다. 이를 보완할 방법이 필요하다. 이 부분은 숙제로 남겨둔다.

### 그래프 출력하기

#### 몸풀기 - 가로 히스토그램 

`0`부터 `9`까지 숫자의 빈도를 구했다. 이제 각 빈도를 히스토그램으로 그릴 차례다. 원하는 문자를 빈도만큼 반복하는 함수로는 `replicate`가 있다.

```haskell
replicate 10 'a' = "aaaaaaaaaa"
replicate 3 1 = [1,1,1]
```

가로 히스토그램이라면 이 `replicate`를 사용해 쉽게 그릴 수 있다. 빈도수 리스트와 `['0'..'9']`를 `zip`하면 빈도수와 숫자의 튜플로 된 리스트를 구할 수 있다. 이 리스트에 `map`을 적용해서 원하는 문자열을 만들면 된다.

```haskell
drawHHistogram xs = map ( \(value,freq) -> [value, '='] ++ (replicate freq '*')) (zip ['0'..'9'] xs) 
```

이제 `unlines`로 문자열 중간에 `\n`을 추가해 반환하게 하면 히스토그램을 `putStr`로 찍어볼 수 있다.

```haskell
drawHHistogram xs = unlines(map ( \(value,freq) -> [value,'='] ++ (replicate freq '*')) (zip ['0'..'9'] xs))
```

##### 가로 히스토그램 한데 모으기

다음은 전체 가로 히스토그램 프로그램이다.

```haskell
-- HHistogram.hs
initialMap x = 0
addMap map key value = \k -> if k==key then value else (map k)
calcDist xs = map (foldl (\map -> \x -> addMap map x ((map x) + 1)) initialMap xs) [0..9]
drawHHistogram xs = unlines(map ( \(value,freq) -> [value,'='] ++ (replicate freq '*')) (zip ['0'..'9'] xs))
histogram xs = drawHHistogram (calcDist xs)
```

실행 결과는 다음과 같다.

```haskell
Prelude> :load HHistogram.hs
[1 of 1] Compiling Main             ( HHistogram.hs, interpreted )
Ok, modules loaded: Main.
*Main> histogram [1,2,3,4,6,1,7,8,9,0]
"0=*\n1=**\n2=*\n3=*\n4=*\n5=\n6=*\n7=*\n8=*\n9=*\n"
*Main> putStr(histogram [1,2,3,4,6,1,7,8,9,0])
0=*
1=**
2=*
3=*
4=*
5=
6=*
7=*
8=*
9=*
*Main>
```

#### 본게임 - 세로 히스토그램

##### `transpose`를 사용해 가로 히스토그램 돌리기

별 고민하지 않고 생각해낼 수 있는 해법으로는 두가지 방법이 있다. 첫번째 방법은 가로 히스토그램을 사용하되 `Data.List.transpose`라는 함수를 사용해 히스토그램을 90도 돌려서 표시하는 방법이다. `transpose`는 다음과 같다.

```haskell
import Data.List
transpose [[1,2,3],[4,5,6]] = [[1,4],[2,5],[3,6]]
{-
 1 2 3           1 4
 4 5 6   <->     2 5
                 3 6
-}
```

돌릴때 도는 방향이 시계방향임에 유의해야 한다. 우리가 원래 그렸던 가로 히스토그램을 돌리면 엉뚱한 히스토그램이 나온다. 따라서 가로 히스토그램의 모양이 다음과 같아야 `transpose`를 써서 변환이 가능하다.

```haskell
    
    =0
   *=1
   *=2
   *=3
****=4
   *=5
  **=6
    =7
    =8
   *=9

<->

    *
    *
    * *
 ******  *
==========
0123456789
```

원래의 가로 히스토그램의 각 줄을 `reverse`해주면 될것 같지만 실제론 그렇지 않다. 히스토그램이세 가장 빈도가 많은 숫자의 빈도수에 맞게 다른 숫자들의 히스토그램에 표시할 `*`나 `=` 앞에 적절히 공백이 들어가야 하기 때문이다. 

넣어야 하는 빈 칸의 개수는 *빈도수 중에 최댓값 - 표시하려는 빈도수*다. 따라서 우선 최댓값을 구해야 한다. 하스켈에는 리스트의 최댓값을 구해주는 `Data.List.maximum` 함수와 `Data.List.maximumBy`가 있다. 우리는 튜플을 비교해야 하기 때문에 비교 함수를 제공해야 하는 `maximumBy`를 써야 한다. 

`maximumBy`의 타입을 보면 `Foldable t => (a -> a -> Ordering) -> t a -> a`라고 되어있다. 일단 잘 모르겠는 (타입 파라미터를 표현하는) `Foldable t`와 `=>`를 무시하면, 실제 두 인자의 타입은 `a -> a -> Ordering`이라는 비교 함수와 `t a`라는 어떤 컨테이너 타입이다.

그럼 `Ordering`은 대체 뭘까? [Hoogle](https://www.haskell.org/hoogle/?hoogle=Ordering)에서 `Ordering`을 찾아보면 `LT`, `GT`, `EQ`라는 이늄으로 값의 대소를 지정할 수 있는 타입임을 알 수 있다. 흠.. 어떻게 `LT` 등을 반환할 수 있을까? 혹시 비교연산자를 사용하면 알아서 잘 해주지는 않을까?

```haskell
*Main Data.List> maximumBy (\x -> \y -> x<=y) [1,2,3]

<interactive>:13:24: error:
    ? Couldn't match expected type ‘Ordering’ with actual type ‘Bool’
    ? In the expression: x <= y
      In the expression: \ y -> x <= y
      In the first argument of ‘maximumBy’, namely
        ‘(\ x -> \ y -> x <= y)’
```

역시나 타입 오류가 난다. 직접 `Ordering`을 반환하는 함수를 작성하는 것도 어렵지는 않다. 하지만 웬지 좀 억울하다. `Ordering`을 반환하는 함수가 없나 [프렐류드](http://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.htm)를 찾아보니 `compare`라는 함수가 있다.

```haskell
compare :: a -> a -> Ordering
```

오호! 심지어 튜플에도 `compare`가 통한다. 거저먹을 수 있을까? 아니다! 애석하게도 하스켈의 튜플 디폴트 비교 함수는 사전식 비교인 것 같다.

```haskell
*Main Data.List> compare ('a',1) ('d',2)
LT
*Main Data.List> compare ('a',100) ('d',2)
LT
```

우리는 앞의 문자는 관계 없이 뒤의 숫자만 비교하고 싶다. 할 수 없이 람다로 튜플에서 두번째 값만 비교하게 `compare`에 넘겨야 할 것이다.

```haskell
*Main Data.List> cmpStat = \(_,v1) -> \(_,v2) -> compare v1 v2
*Main Data.List> cmpStat ('a',100) ('d',3)
GT
```

이제 빈도수 튜플 리스트를 `maximumBy`을 사용해 구한 최댓값을 이용해 `(숫자,공백수,빈도수)`로 변환하자. 

```haskell
import Data.List
addSpace xs = let 
  cmpStat = \(_,v1) -> \(_,v2) -> compare v1 v2
  (_, mx) = maximumBy cmpStat xs
in 
  map (\(x,y) -> (x, mx-y, y)) xs
```

이제 `addSpace`를 테스트해볼 수 있다.

```haskell
*Main> addSpace [('0',1),('1',10),('2',3),('4',0)]
[('0',9,1),('1',0,10),('2',7,3),('4',10,0)]
```

여기까지 하고 나면 가로 히스토그램을 만드는 일은 쉽다. 앞에서 본 `drawHHistogram`의 출력 순서를 뒤집으면서 공백만 `replicate`를 사용해 필요한 만큼 추가하면 된다.

```haskell
drawHHistogramReverse xs = map ( \(value,nSpace,freq) -> (replicate nSpace ' ') ++ (replicate freq '*') ++ ['=', value]) xs
```

이렇게 그린 히스토그램을 돌리면 원하는 세로 히스토그램이 나온다.

##### 전체 코드

`transpose`를 사용해 전체를 돌리는 방식으로 그린 히스토그램 코드는 다음과 같다.

```haskell
-- VHistogram.hs
import Data.List

initialMap x = 0
addMap map key value = \k -> if k==key then value else (map k)
calcDist xs = map (foldl (\map -> \x -> addMap map x ((map x) + 1)) initialMap xs) [0..9]

addSpace xs =
  let
    cmpStat = \(_,v1) -> \(_,v2) -> compare v1 v2
    (_,mx) = maximumBy cmpStat xs
  in
    map (\(x,y) -> (x, mx-y, y)) xs

drawHHistogramReverse xs = map (\(value,nSpace,freq) -> (replicate nSpace ' ') ++ (replicate freq '*') ++ ['=', value]) xs

histogram xs = transpose(drawHHistogramReverse(addSpace (zip ['0'..'9'] (calcDist xs))))
```

> ##### 들여쓰기 규칙
>
> 하스켈은 들여쓰기를 어느정도 강제한다. 파이썬보다는 약간 느슨하다고 할 수도 있지만, 기본 규칙은 다음과 같다.
>
> **다른 식을 이루는 부분식은 원래의 식이 시작된 지점보다 더 뒤로 들여쓰기 해야 한다**
>
> 이 규칙으로 인해 **같은 수준의 식의 시작 부분은 모두 동일해야 한다**라는 부가 규칙이 생긴다. 
>
> `let ... in ...`와 같은 식의 경우 함수 정의에 들어가면 위치를 잡기가 애매하다. 나(현석)는 `addSpace`에서와 같은 방식을 택했지만 다른 방식을 택할 수도 있다. 여러 방법에 대해서는 하스켈 위키북의 [들여쓰기 관련 내용](https://en.wikibooks.org/wiki/Haskell/Indentation)을 살펴보라. 다른 여러 코딩 규범에 대해서는 하스켈 위키의 [프로그래밍 가이드라인](https://wiki.haskell.org/Programming_guidelines)을 보라.

이 코드를 실행한 결과는 다음과 같다.

```haskell
E:\blog\example\haskell\cis194\03_recursion
λ ghci VHistogram.hs
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from C:\Users\hyunsok\AppData\Roaming\ghc\ghci.conf
[1 of 1] Compiling Main             ( VHistogram.hs, interpreted )
Ok, modules loaded: Main.
*Main> histogram [1,4,5,4,6,6,3,4,2,4,9]
["    *     ","    *     ","    * *   "," ******  *","==========","0123456789"]
*Main> putStr(unlines(histogram [1,4,5,4,6,6,3,4,2,4,9]))
    *
    *
    * *
 ******  *
==========
0123456789
*Main>
```

#### 돌리지 않고 바로 만들기

가로 히스토그램을 만들고 `transpose`하면 비용이 많이 든다. 다음과 같은 방법으로 세로로 히스토그램을 바로 만들 수 있고 비용도 작을 것이다. 도수 분포가 튜플의 리스트로 되어있어서 검색에 선형 시간이 걸림을 감안해도 (가로히스토그램구축+transpose) 비용이 다음과 같이 직접 세로 히스토그램을 구축하는 비용보다 많이 들 가능성이 크다. 물론 진짜 성능은 벤치마크를 해봐야 할 것이다. 또는 맵이나 배열 등을 사용하면 성능을 향상시킬 수 있을 것이다.

1. 최대 빈도수부터 1까지 `x`를 1씩 감소시켜 가면서
      1. 현재 `x`에 해당하는 `*`를 그려야 하는지 도수 분표 리스트에서 각 문자의 빈도수를 찾아서 결정해서 그려야 하는 경우 '*'를, 그리지 않아야 하는 경우 ' '를 넣어서 문자열을 만든다.
2. `=` 10개로 이뤄진 문자열을 추가한다.
3. `"0123456789" 문자열을 추가한다.

구현은 나중에 심심할때 다시 추가하겠다. 일단은 독자들에게 숙제로 남긴다.