---
layout:     post
title:      "[하스켈 기초][CIS194] 대수적 데이터 타입(Algebraic Data Type) 연습문제 풀이"
date:       2018-01-01 21:45:00
summary:    "대수적 데이터 타입 소개와 재귀적 데이터 구조 소개 연습문제 풀이"
categories: Haskell CIS194 Algebraic Data Type Exercise Solution
---

[CIS194 2강 대수적 데이터 타입](http://enshahar.com/haskell/cis194/algebraic/data/type/2017/12/24/cis194-adt/)에 있는 연습문제 풀이입니다.

## `parseMessage`와 `parse`

### `parseMessage :: String -> LogMessage`

한 줄의 로그를 받아서 `LogMessage`로 분석하는 도구다. 한 줄의 텍스트를 입력으로 받으면 일단 단어 단위로 분리해야 한다. 다행히 하스켈에는 `words`라는 함수가 있다. `words`의 타입은 `ghci`에서 `:type` 명령을 사용하거나 하스켈 문서에서 `words`를 찾아보면 쉽게 알 수 있다.

```haskell
Prelude> :type words
words :: String -> [String]
```

이렇게 단어별로 분리한 `[String]` 타입의 리스트가 있다면 첫번째 단어는 `I`,`E`,`W`여야 하고 다른 단어여서는 안된다. 일단 여기까지를 코드로 구현해 보자.

```haskell
{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage  
parseMessage s = case (words s) of        -- s를 단어별로 나눠서 분석함
                  []    -> Unknown s      -- 0단어만 있는 경우
                  (_:[]) -> Unknown s     -- 1단어만 있는 경우
                  (x:y:zs)                -- 2단어 이상 있는 경우
                     | x == "I" -> -- 채워 넣어야 할 부분
                     | x == "W" -> -- 채워 넣어야 할 부분
                     | x == "E" -> -- 채워 넣어야 할 부분
                     | otherwise -> Unknown s   -- I, W, E가 첫 단어가 아닌 경우
```

`I`나 `W`인 경우는 쉽다. 두번째 단언인 `y`에 타임스탬프가 정수로 들어가 있으므로 `read`함수를 써서 정수로 변환하고 적절히 `LogMessage`를 만들어주면 된다.

```haskell
parseMessage :: String -> LogMessage  
parseMessage s = case (words s) of        -- s를 단어별로 나눠서 분석함
                  []    -> Unknown s      -- 0단어만 있는 경우
                  (_:[]) -> Unknown s     -- 1단어만 있는 경우
                  (x:y:zs)                -- 2단어 이상 있는 경우
                     | x == "I" -> LogMessage Info (read y::Int) (unwords(zs))
                     | x == "W" -> LogMessage Warning (read y::Int) (unwords(zs))
                     | x == "E" -> -- 채워 넣어야 할 부분
                     | otherwise -> Unknown s   -- I, W, E가 첫 단어가 아닌 경우

```

한가지 남은 부분은 첫 단어가 `E`인 경우다. `E`에서는 세번째 단어에 타임스탬프가 들어가야 한다. 따라서 `zs`라는 리스트를 다시 `case` 식으로 분석해서 `zs`에 있는 첫번째 원소에서 타임스탬프를 추출해야 한다. 이를 구현해 넣은 최종 결과는 다음과 같다.

```haskell

parseMessage :: String -> LogMessage  
parseMessage s = case (words s) of        -- s를 단어별로 나눠서 분석함
                  []    -> Unknown s      
                  (_:[]) -> Unknown s     -- 0단어 또는 1단어만 있는 경우
                  (x:y:zs) 
                     | x == "I" -> LogMessage Info (read y::Int) (unwords(zs))
                     | x == "W" -> LogMessage Warning (read y::Int) (unwords(zs))
                     | x == "E" -> case zs of  -- E가 첫 단어인 경우
                                    [] -> Unknown s
                                    (z:zss) -> LogMessage (Error (read y::Int)) (read z::Int) (unwords zss)
                     | otherwise -> Unknown s   -- I, W, E가 첫 단어가 아닌 경우
```

한가지 아쉬운 점은 `read y::Int`등 `read`를 사용한 연산에서 잘못된 데이터가 들어온 경우 예외(exception)가 발생하는데 아직 예외 처리를 배우지 못했다는 점과, `unwords`로 얻은 문자열이 실제로 로그 파일에서 읽은 문자열과 달라질 수 있다는 점이다. 일단은 이 상태로 넘어가자. 이에 대해 수정하는 것은 연습문제로 남겨둔다.

###  parse :: String -> [LogMessage]

이제 `parseMessage`가 있으면 쉽게 `parse`를 만들 수 있다. 문자열을 `\n`으로 분리해서 각 줄이 원소인 리스트로 만드는 함수는 `lines`이다. 리스트의 모든 원소를 변환해주는 함수로 `map`이 있다. 앞으로 자주 쓰게 될 것이므로 배워두도록 하자.

```haskell
map :: (a -> b) -> [a] -> [b]
```

`map`을 유한 또는 무한 리스트에 적용한 결과는 다음과 같다.

```haskell
map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
map f [x1, x2, ...] == [f x1, f x2, ...] 
```

`map`과 `parseMessage`가 있으면 쉽게 `parse`를 구현할 수 있다. 

```haskell
parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)
```

## 메시지 2진 검색 트리

### `insert :: LogMessage -> MessageTree -> MessageTree`

일단 `Unknown`을 받으면 원래 트리를 그냥 반환한다.

```haskell
insert (Unknown _) tree = tree
```

로그가 `Unknown`이 아닌데, 이진 트리가 `Leaf`(빈 트리)라면 받은 `LogMessage`를 루트로 하는 트리를 만들어야 한다.

```haskell
insert msg Leaf = Node Leaf msg  Leaf
```

로그가 `Unknown`이 아니고, 이진 트리도 `Leaf`가 아니라면 어떻게 해야 할까? 2진 트리의 규칙을 생각해 보자. 로그의 타임 스탬프가 트리 루트에 있는 로그의 타임스탬프보다 작다면 로그는 왼쪽 자식 트리에 속해야 하고, 로그의 타임 스탬프가 트리 루트에 있는 로그의 타임스탬프보다 크다면 로그는 오른쪽 자식 트리에 속해야 한다. 이때 왼쪽이나 오른쪽 자식 트리에 속하게 만드는 것은 `insert`함수를 다시 왼쪽이나 오른쪽 트리에 적용하면 된다. 이때 `insert`로 만든 새 자식 트리를 가지고 새로운 트리를 구성해야 함에 유의하라. 메시지의 타임스탬프 값이 노드에 있는 메시지의 타임스탬프와 같은 경우에는 왼쪽이나 오른쪽 트리 어느쪽에 들어가도 된다. 그냥 새 메시지를 노드의 왼쪽 트리에 넣자.

```haskell
insert msg@(LogMessage _ ti _) (Node left msg2@(LogMessage _ ti2 _) right)
    | ti <= ti2 = Node (insert msg left) msg2 right
    | ti > ti2 = Node left msg2 (insert msg right)
```

이를 종합하면 다음과 같다.

```haskell
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ti _) (Node left msg2@(LogMessage _ ti2 _) right)
    | ti <= ti2 = Node (insert msg left) msg2 right
    | ti > ti2 = Node left msg2 (insert msg right)
```

제대로 구현하자면 균형 2진 트리가 되게 루트의 양 노드의 트리 깊이를 조절해줘야 하지만 연습문제로 남겨둔다.

### `build :: [LogMessage] -> MessageTree`

[1강 연습문제](http://enshahar.com/haskell/cis194/introduction/exercise/solution/2017/12/11/cis194-introhaskell-exanswer/)에서는 리스트의 모든 원소를 더하는 `sumList` 함수를 만들었었다.

```haskell
sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs
```

이 함수를 연산자 방식이 아니라 함수 호출 방식으로 쓰면 다음과 같다.

```haskell
sumList' :: [Integer] -> Integer
sumList' [] = 0
sumList' (x:xs) = (+) x  (sumList xs)
```

`0`을 `Laef`로, `(+)`를 `insert`로 바꿔서 생각해보면 쉽게 다음같은 함수를 만들 수 있다.

```haskell
build' :: [LogMessage] -> MessageTree
build' [] = Leaf
build' (m::ms) = insert m (build' ms)
```

`sumList'`과 `build'`사이에 공통 패턴이 보이나? 어떤 리스트에 대해 리스트가 널일 경우 반환할 초깃값(꼭 리스트의 원소 타입과 같은 타입일 필요가 없다)과 반복 적용할 연산(피연산자로 초깃값과 같은 타입의 값과 리스트 원소 타입과 같은 타입의 값을 받아야 한다)이 주어지면 재귀적으로 연산을 반복적용해주는 함수를 만들 수 있다. 함수 프로그래밍에서는 이런 함수를 `fold`라고 부른다. 이때 연산자의 좌항과 우항 중 어느쪽에 초깃값과 같은 타입이 들어가느냐에 따라서 `fold`의 방향이 결정된다. 연산자 우항 타입이 초깃값 타입과 같은 경우 `foldr`, 연산자 좌항 타입이 초깃값 타입과 같은 경우 `foldl`이라고 부른다.

```haskell
foldr f z []     = z                   -- 리스트가 비어있으면 초깃값을 반환한다
foldr f z (x:xs) = f x (foldr f z xs)  -- 원소가 있으면 `f`의 좌항에 원소를, 우항이 `foldr`을 나머지 리스트에 재귀적으로 적용한 결과를 전달한다
 
-- foldr과 전달하는 방향이 반대다.
foldl f z []     = z                  
foldl f z (x:xs) = foldl f (f z x) xs
```

`foldr`이나 `foldl`을 안다면 `build`를 다음과 같이 간단히 구현할 수 있다. 마찬가지로 `sumList`도 쉽게 `foldr`이나 `foldl`을 사용해 구현할 수 있다.

```haskell
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

sumList: [Integer] -> Integer
sumList = foldr (+) 0
```

이때 `build`선언을 다음과 같이 하지 않았다는 사실에 유의하라. `l`은 이 경우 있으나 마나하기 때문에 쓰지 않아도 된다. 물론 명시해도 문제는 없다. 기존 명령형 언어의 함수 선언에 익숙한 사람이라면 처음에는 명시하는 쪽이 이해하기 쉬울 것이다. 하지만 함수의 타입과 인자 적용 순서를 잘 따져보면 앞의 선언과 다음 선언이 같음을 쉽게 알 수 있다.

```haskell
build :: [LogMessage] -> MessageTree
build l = foldr insert Leaf l
```

### `inOrder :: MessageTree -> [LogMessage]`

인자로 받은 `MessageTree`가 `Leaf`라면 아무것도 할 필요가 없다. 타입을 맞춰야 하니 빈 리스트를 반환하자. 함수형 프로그래밍에서는 타입을 중심으로 생각해 보면 택할 수 있는 구현의 가짓수가 줄어든다. 따라서 타입을 중심으로 생각하는 습관을 들이는 것이 좋다.

```haskell
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
```

`Node`를 인자로 받는다면 어떻게 해야 할까? 오른쪽 트리와 왼쪽 트리에 대해 각각 정렬된 로그 메시지 리스트가 있다면 그 둘 사이에 현재 노드의 메시지를 넣는 것으로 충분하다. 이를 코드로 만들면, 일단 오른쪽 트리에 `inOrder`를 적용한 리스트에 노드가 담고 있는 메시지를 추가하고, 그 뒤에 왼쪽 트리에 `inOrder`를 적용한 리스트를 덧붙이는 형태가 된다. 재귀적인 문제 해결에서는 가장 간단한 단계의 해법(여기서는 `Leaf`인 경우)을 잘 정의하고, 재귀적으로 작은 문제들이 다 풀렸다고 가정한 경우 전체 답을 어떻게 구성해야 하는가에만 신경쓰면 되기 때문에 문제 해결이 더 쉽다. 

리스트 뒤에 리스트를 붙이는 것은 `++`로 된다. 리스트 뒤에 원소를 붙이는 것은 원소를 `[]`로 감싸서 리스트로 만들면(또는 `:[]`를 해서 리스트로 만들면) `++`로 해결할 수 있다.

```haskell
inOrder Node left msg right = (inOrder left) ++ [msg] ++ (inOrder right)
```

이를 모두 합치면 다음과 같다.

```haskell
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder Node left msg right = (inOrder left) ++ [msg] ++ (inOrder right)
```

### `whatWentWrong :: [LogMessage] -> [String]`

지금까지 사용해온 리스트에 대한 재귀를 사용하면서 조건에 따라 처리하면 된다.

```haskell
whatWentWrong' :: [LogMessage] -> [String]
whatWentWrong' [] = []
whatWentWrong' ((LogMessage (Error v) _ s):xs) | v >= 50 = s : whatWentWrong' xs
whatWentWrong' ((LogMessage _ _ _):xs) = whatWentWrong' xs
whatWentWrong' ((Unknown _):xs) = whatWentWrong' xs
```

다른 방법으로는 로그메시지 리스트에서 조건을 만족하는 원소만 걸러낸 다음에 `map`을 사용해 `LogMessage`에서 메시지만을 빼내는 방법이 있다. 하스켈에는 `filter :: (a -> Bool) -> [a] -> [a]`라는 함수가 있다. 일단 `LogMessage`인지 판단해 걸러내고, 그 후 메시지 타입이 `Error`이면서 심각도가 50이상인지 확인하자. 참고로 이런식으로 `Bool`을 반환하는 함수를 술어 함수(predicate function)나 술어라고 부르기도 한다.

하스켈 언어 명세인 [하스켈 레포트](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-460003.13)를 찾아보면 패턴 매칭을 람다에서도 사용할 수 있다. 따라서 `case` 없이도 람다에서 패턴매칭을 사용해 값 추출 함수를 만들 수 있다(조건 함수에서 람다를 사용하는 부분은 람다에 바로 패턴매칭을 사용하는 형태로 어떻게 할 수 있는지 모르겠다. 앞으로 공부하다 알게 되면 정리해 보기로 한다.) 

가독성을 위해 조건함수들을 람다로 직접 `whatWentWrong`에 사용하는 대신 별도로 정의하자.

```
isError :: LogMessage -> Bool
isError (Unknown _) = False
isError (LogMessage (Error _) _ _) = True
isError (LogMessage _ _ _) = False

getSeverity :: LogMessage -> Int
getSeverity = \(LogMessage (Error s) _ _) -> s

getString :: LogMessage -> String
getString = \(LogMessage _ _ s) -> s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map getString (filter (\x->getSeverity(x) >= 50) (filter isError xs))
```

한편 합성함수를 만들어주는 `(.)`를 사용하면 `whatWentWrong`을 다음과 같이 정의할 수도 있다. `.`는 오른쪽 함수부터 적용시켜주며, 하스켈에서 중위 연산자로 사용하는 기호(여기서는 `.`) 연산자의 우선순위는 다른 함수 적용보다 낮다는 사실을 활용하면 괄호 없이도 더 깔끔하게 코드를 만들 수 있다. 이런식의 코드를 봐도 멘붕이 오지 않도록 적응할 필요가 있다.

```haskell
whatWentWrong'' :: [LogMessage] -> [String]
whatWentWrong'' = map getString . filter (\x->getSeverity(x) >= 50) . filter isError
```

이렇게 `.`로 연속적으로 (괄호 없이) 합성된 함수를 일을 때는 오른쪽부터 왼쪽으로 `.`를 찾아가면서 끊어서 읽는다.

1. `filter isError` : 아. 리스트에 `isError`라는 술어를 적용해 걸러내는구나. 이제 리스트에는 `isError`를 만족하는 원소들만 남겠구나.
2. `(\x->getSeverity(x) >= 50)` : `x`인자로 받은 원소에 대해 `getSeverity`를 적용하고, 그 결과가 50이상인지 검사하는 람다로구나.
3. `filter (\x->getSeverity(x) >= 50)` : 심각도가 50이상인 원소만 리스트에 남기는 구나.
4. `map getString` : `getString`을 리스트의 모든 원소에 적용하는구나. `LogMessage`에서 문자열만 뽑아낸 리스트를 만들겠구나.

여기서 한단계 더 나가서 `\x->getSeverity(x) >= 50`도 함수 합성을 이용해 다음과 같이 만들 수 있다. 여기서 `(<=)`로 전위 호출 형태로 사용하는 경우 50이 첫번째 인자기 때문에 `>=`가 아니라 `<=`를 사용해서 50보다 두번째 인자가 더 크거나 같은지 검사해야 한다는 점에 유의하라.

```haskell
whatWentWrong''' :: [LogMessage] -> [String]
whatWentWrong''' = map getString . filter (((<=) 50) . getSeverity) . filter isError
```

#### `whatWentWrong`, `whatWentWrong'`, `whatWentWrong''`, `whatWentWrong'''`이 같은지 검사

이런식으로 같은 역할을 하는 함수를 여럿 만든 경우 테스트케이스를 각 함수에 정의한 결과를 서로 비교하면 각 함수가 내놓는 결과가 같은지 비교할 수 있다. 아직 테스트 프레임워크를 따로 사용하지 않으므로 그냥 하스켈의 `==`를 사용해 리스트를 비교해보자.

강의 숙제에 첨부된 `log.hs`를 보면 테스트를 위한 `testWhatWentWrong` 함수와 `error.log` 로그 파일이 있다. 이를 사용해 우리가 만든 네 함수의 결과를 비교해보자. REPL(`ghci`)에서 `LogAnalysis.hs`를 로딩한 상태에서 다음을 수행해 보라. `testWhatWentWrong`의 타입이 `IO [String]`이어서 `<-`를 사용해 `[String]` 타입의 값을 각 변수에 바인딩했다.

```haskell
E:\blog\example\haskell\cis194\02_adt
λ ghci LogAnalysis.hs
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from C:\Users\hyunsok\AppData\Roaming\ghc\ghci.conf
[1 of 2] Compiling Log              ( Log.hs, interpreted )
[2 of 2] Compiling LogAnalysis      ( LogAnalysis.hs, interpreted )
Ok, modules loaded: Log, LogAnalysis.
*LogAnalysis> l1 <- testWhatWentWrong parse whatWentWrong "error.log"
*LogAnalysis> l2 <- testWhatWentWrong parse whatWentWrong' "error.log"
*LogAnalysis> l3 <- testWhatWentWrong parse whatWentWrong'' "error.log"
*LogAnalysis> l4 <- testWhatWentWrong parse whatWentWrong''' "error.log"
*LogAnalysis> l1 == l2
True
*LogAnalysis> l3 == l4
True
*LogAnalysis> l1 == l3
True
*LogAnalysis>
```

`==`이 동치관계(equivalence relation)이므로 이렇게만 비교하면 충분하다.


