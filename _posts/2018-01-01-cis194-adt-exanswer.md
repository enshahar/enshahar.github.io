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



