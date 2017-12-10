---
layout:     post
title:      "[하스켈 기초] 하스켈 공부법"
date:       2017-12-09 22:47:00
summary:    "[하스켈기초] 하스켈 익스퍼트 비기너되기"
categories: Haskell Study
---

하스켈을 공부하려 합니다. 뒤적뒤적 해보다 [하스켈 학습 가이드](https://github.com/bitemyapp/learnhaskell/blob/master/guide-ko.md)라는 문서를 만났습니다. 일단은 여기 나온 내용을 바탕으로 한번 빠르게 익스퍼트 비기너가 되기 위해 노력해보려 합니다. 대충 다음과 같은 단계를 거치는 것 같습니다.

- 하스켈 도구로 [Stack](http://haskellstack.org/) 설치. 하스켈 홈페이지에 있는 하스켈 플랫폼을 초보에겐 권장하지 않네요. 각종 도구를 잘 알기 어렵고 등등 이야기가 있다는데 자세히는 안 살펴봤습니다. 난 초보니까 맹목적으로 이 문서를 따라해 보려 합니다.
- [Yorgey의 CIS194 강의](http://www.seas.upenn.edu/%7Ecis194/spring13/lectures.html) 듣기. 단 프로그래머가 아닌 사람은 [톰슨의 "하스켈-함수형 프로그래밍 기법](http://www.haskellcraft.com/craft3e/Home.html) 책을 통해 기초를 잡으라고 합니다. 기회가 되면 이 책도 다뤄보죠(3판이 나왔는데 저는 2판을 가지고 있습니다.)
- [Data 61의 강의](https://github.com/data61/fp-course) 듣기. Data 61은 NICTA라는 호주 연구 조직에서 만든 회사입니다. 제가 브리즈번사는데 이민올때는 몰랐는데 알고보니 골수 함수 프로그래머들이 몇몇 자리잡고 있는 동네더군요(안타깝게도 그렇다고 그 아저씨들과 교류가 있거나 하지는 않습니다. 전 C++/자바 프로그래머고 먹고 살기 바빠서T.T) 어쨌든 "Data 61 하스켈 강의" 또는 "NICTA 하스켈 강의"라고 하면 이 강의를 말합니다. Yorgey의 강의를 다 듣고 나서 이 강의를 들으라고 하네요.
- 위 두 강의를 마치고 나면 [Bryan O'Sullivan의 강의](https://github.com/bos)를 들으랍니다.

기타 내용도 있지만 일단 익스퍼트 비기너라면 대충 여기까지 마치고 만족하면 될것 같습니다. 그중 흥미로운건 나중에 따로..


## Stack 설치

Stack 홈페이지 가서 자기 플랫폼에 맞는 설치 방법을 찾아서 설치하십시오. 비개발자는 더 친절한 안내를 알아서 찾아보시기 바랍니다. 

저는 윈도우 사용자라서 [윈도우용 64비트 설치 프로그램](https://www.stackage.org/stack/windows-x86_64-installer)을 다운받아 설치했습니다.

설치하고 나면 `stack`이라는 툴이 생깁니다. 이 툴을 사용해 프로젝트 생성,빌드,테스트 등이 가능한 모양입니다. 대충 홈피에 있는 기본적 사용법을 정리해 봅시다.

### 기본 사용법 - 프로젝트 시작

```
stack new 프로젝트이름
cd 프로젝트이름
stack setup
stack build
stack exec 프로젝트이름-exe
```

이런식이랍니다. 한번 해보죠.

#### stack new hello

프로젝트가 필요할지 잘 모르겠습니다만, 일단 hello라고 이름을 정했습니다. `stack new hello`를 실행해 봅시다. 다음과 같은 모습을 볼 수 있습니다. 

```
E:\blog\example\haskell
λ stack new hello
Downloading template "new-template" to create project "hello" in hello\ ...

The following parameters were needed by the template but not provided: author-email, author-name, category, copyright, github-username
You can provide them in C:\sr\config.yaml, like this:
templates:
  params:
    author-email: value
    author-name: value
    category: value
    copyright: value
    github-username: value
Or you can pass each one as parameters like this:
stack new hello new-template -p "author-email:value" -p "author-name:value" -p "category:value" -p "copyright:value" -p "github-username:value"

Looking for .cabal or package.yaml files to use to init the project.
Using cabal packages:
- hello\

Selecting the best among 11 snapshots...

Downloaded lts-9.17 build plan.
(이하 생략)
```

관련 패키지 등을 모두 다운로드하고 나면 `hello` 디렉터리 아래 다음과 같은 구조의 프로젝트가 생성됩니다.

```
hello
├── LICENSE (라이선스 관련 정보)
├── Setup.hs  (프로젝트 설정 파일)
├── app
│   └── Main.hs (만들어질 실행파일에 들어가야 할 모듈)
├── hello.cabal (패키징 관련 설정 설정파일)
├── src
│   └── Lib.hs  (Main.hs에서 쓸 메인 함수가 여기 정의됨)
├── stack.yaml (스택 설정 파일)
└── test
    └── Spec.hs (테스트 스펙)
```

각 파일 이름 옆에 설명을 달아뒀습니다(파일 내용을 보고 이러리라 추측한 내용입니다. 혹시 틀릴수도 있습니다. 나중에 배우다 보면 정확하게 알게 되겠죠).

[스택 사용자 가이드](https://docs.haskellstack.org/en/stable/GUIDE/)를 보면 더 많은 내용이 있습니다. 이것도 나중에 필요해지면 정리를 하겠습니다.

#### stack setup 

`hello`라는 디렉터리로 `cd`해서 `stack setup`을 하면 관련 패키지와 ghc 등을 모두 다운받아 설정해줍니다. 제가 사는 집은 네트워크가 느려서(아니 여보쇼 21세기에 ADSL2+라니! 거기에 이론상 속도 절반도 안 나오다니!)... 한참 걸립니다만 한국에선 그렇지 않으리라 봅니다.

```
E:\blog\example\haskell\hello
λ stack setup
(ghci 다운로드부터 시작해서 여러가지 패키지 다운로드/설치)

###################################################################
#                                                                 #
#                                                                 #
#                   C   A   U   T   I   O   N                     #
#                                                                 #
#                  This is first start of MSYS2.                  #
#       You MUST restart shell to apply necessary actions.        #
#                                                                 #
#                                                                 #
###################################################################


stack will use a sandboxed GHC it installed
For more information on paths, see 'stack path' and 'stack exec env'
To use this GHC and packages outside of a project, consider using:
stack ghc, stack ghci, stack runghc, or stack exec

E:\blog\example\haskell\hello
```

이런! MSYS를 설치해 버리시네요. 설명한대로 명령창을 닫고 다시 띄웁니다.

#### stack build

빌드해봅시다!

```
E:\blog\example\haskell\hello
λ stack build
(...생략...)
Preprocessing executable 'hello-exe' for hello-0.1.0.0...
[1 of 1] Compiling Main             ( app\Main.hs, .stack-work\dist\ca59d0ab\build\hello-exe\hello-exe-tmp\Main.o )
Linking .stack-work\dist\ca59d0ab\build\hello-exe\hello-exe.exe ...
hello-0.1.0.0: copy/register
Installing library in
E:\blog\example\haskell\hello\.stack-work\install\fab39206\lib\x86_64-windows-ghc-8.0.2\hello-0.1.0.0-KwseWZXCZph1LcfBbJuRhm
Installing executable(s) in
E:\blog\example\haskell\hello\.stack-work\install\fab39206\bin
Registering hello-0.1.0.0...

E:\blog\example\haskell\hello
λ
```

별 문제 없이 빌드 됩니다. 중간에 보면 만들어진 exe 파일 이름이 `hello-exe.exe`인것 같습니다. 맨 처음에 전체 프로젝트 실행 단계에서 `stack exec 프로젝트이름-exe`라고 썼던 것과 맞아 떨어지는 이름이네요.

#### statkc exec hello-exe

실행해보죠.

```
E:\blog\example\haskell\hello
λ stack exec hello-exe
someFunc
```

잘 되는거 맞나 싶긴 하지만 어쨌든 문제없이 실행됩니다. 

한번 `src\Lib.hs`를 고쳐서 "Hello, World!"를 찍게 만들어 봅시다.

```
E:\blog\example\haskell\hello
λ vi src\Lib.hs (편집함)
λ type src\Lib.hs (편집한 결과를 출력해봄)
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "Hello, World!"
```

```
E:\blog\example\haskell\hello
λ stack build
hello-0.1.0.0: unregistering (local file changes: src\Lib.hs)
hello-0.1.0.0: build (lib + exe)
Preprocessing library hello-0.1.0.0...
[2 of 2] Compiling Lib              ( src\Lib.hs, .stack-work\dist\ca59d0ab\build\Lib.o )
Preprocessing executable 'hello-exe' for hello-0.1.0.0...
[1 of 1] Compiling Main             ( app\Main.hs, .stack-work\dist\ca59d0ab\build\hello-exe\hello-exe-tmp\Main.o ) [Lib changed]
Linking .stack-work\dist\ca59d0ab\build\hello-exe\hello-exe.exe ...
hello-0.1.0.0: copy/register
Installing library in
E:\blog\example\haskell\hello\.stack-work\install\fab39206\lib\x86_64-windows-ghc-8.0.2\hello-0.1.0.0-KwseWZXCZph1LcfBbJuRhm
Installing executable(s) in
E:\blog\example\haskell\hello\.stack-work\install\fab39206\bin
Registering hello-0.1.0.0...

E:\blog\example\haskell\hello
λ stack exec hello-exe
Hello, World!

E:\blog\example\haskell\hello
λ
```

멋집니다! 

#### stack ghci

ghci는 하스켈 표준 구현인 글래스고 하스켈의 REPL입니다. 간단한 코드를 테스트해볼 때 자주 사용하게 될 것 같습니다(물론 IDE나 텍스트 에디터의 REPL 지원 기능을 활용할 가능성도 있습니다).

```
E:\blog\example\haskell\hello
λ stack ghci
The following GHC options are incompatible with GHCi and have not been passed to it: -threaded
Configuring GHCi with the following packages: hello
Using main module: 1. Package `hello' component exe:hello-exe with main-is file: E:\blog\example\haskell\hello\app\Main.hs
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Lib              ( E:\blog\example\haskell\hello\src\Lib.hs, interpreted )
[2 of 2] Compiling Main             ( E:\blog\example\haskell\hello\app\Main.hs, interpreted )
Ok, modules loaded: Lib, Main.
Loaded GHCi configuration from C:\Users\hyunsok\AppData\Local\Temp\ghci26900\ghci-script
*Main Lib> "Hello, World"
"Hello, World"
*Main Lib><ctrl-D>
Leaving GHCi.
```

컨트롤-D를 누르면 ghci에서 나올 수 있습니다.

다음부터는 진지하게(?) 하스켈을 공부해 보겠습니다. 우선 CIS194의 첫 순서로 [하스켈 소개](/haskell/cis194/introduction/2017/12/09/cis194-introhaskell/) 부터요. 

