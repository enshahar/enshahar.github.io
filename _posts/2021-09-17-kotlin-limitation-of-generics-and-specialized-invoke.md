---
layout:     post
title:      "[코틀린] 코틀린 제네릭 클래스의 타입 특화 제약과 invoke를 통한 해결방법"
date:       2021-09-17 11:10:00
summary:    "코틀린 제네릭 타입의 타입 특화 관련 문제점과 invoke와 오버로드 해소를 통해 이런 문제점을 어떻게 우회할 수 있는지 설명함"
categories: Haskell CIS194 Folds Monoids
---

# 코틀린 제네릭스의 타입 특화 제약과 invoke를 통한 해결 방법

## 제네릭스의 타입 특화 제약

코틀린에서는(자바에서도) 클래스 이름은 같지만 제네릭 타입 파라미터만 다른 클래스들을 정의할 수 없다. 

```
$cat Foo.kt
class FOO<T:Any> { fun doit() { println("Any") } }
class FOO<T:Number> { fun doit() { println("Number") } }
class FOO<T:Int> { fun doit() { println("Int") } }

$kotlinc Foo.kt
Foo.kt:1:7: error: redeclaration: FOO
class FOO<T:Any> { fun doit() { println("Any") } }
      ^
Foo.kt:2:7: error: redeclaration: FOO
class FOO<T:Number> { fun doit() { println("Number") } }
      ^
Foo.kt:3:7: error: redeclaration: FOO
class FOO<T:Int> { fun doit() { println("Int") } }
      ^
```

스크립트에서는 클래스 재정의 에러는 안나지만(아마도 컴파일러가 스크립트 모드일 때는 줄마다 다른 패키지 안에 해당 클래스의 정의를 넣기 때문이 아닌가 싶다), 타입 소거 때문에 다양한 타입 파라미터를 가지는 가장 적합한 타입 파라미터를 못 찾아줘서 타입별 특화(specialization)가 안된다.

```
$cat Foo.kts
class FOO<T:Any> { fun doit() { println("Any") } }
class FOO<T:Number> { fun doit() { println("Number") } }
class FOO<T:Int> { fun doit() { println("Int") } }

FOO<String>().doit()
FOO<Int>().doit()
FOO<Double>().doit()

$kotlinc -script Foo.kts
Int
Int
Int
Test.kts:3:13: warning: 'Int' is a final type, and thus a value of the type parameter is predetermined
class FOO<T:Int> { fun doit() { println("Int") } }
            ^
```

## 함수 오버로딩 해소(overloading resolution)를 활용한 최적 타입 결정

이럴때는 함수 오버로딩을 사용하면 제네릭 함수의 타입을 잘 알아맞춰주는 것 같다.

```
>>> fun <T1:Any, T2:Any> bar(v1:T1,v2:T2) { println("AA: ${v1 to v2}") }
>>> fun <T1:Number, T2:Any> bar(v1:T1,v2:T2) { println("NA: ${v1 to v2}") }
>>> fun <T1:Any, T2:Number> bar(v1:T1,v2:T2) { println("AN: ${v1 to v2}") }
>>> fun <T1:Number, T2:Number> bar(v1:T1,v2:T2) { println("NN: ${v1 to v2}") }
>>> bar(1,1)
NN: (1, 1)
>>> bar(1,"test")
NA: (1, test)
>>> bar("test","test")
AA: (test, test)
>>> bar("test",1)
AN: (test, 1)
```

## `invoke`와 동반객체를 사용한 특화 타입 객체 생성 방법

동반객체 안에 `invoke`를 정의하면 객체 생성시 다양한 타입에 따라 특화시키는 일도 가능하다. 공통 기능을 인터페이스로 뽑아두고 이를 상속해 타입별로 특화된 기능을 제공할 수도 있다.

```
$ cat Foo2.kt
abstract class FOO {
  abstract fun doit()
    companion object {
      operator fun <T:Any> invoke(v:T) = object: FOO() { override fun doit() { println("Any: ${v}") } }
      operator fun <T:Number> invoke(v:T) = object: FOO() { override fun doit() { println("Number: ${v}") } }
      operator fun <T:Int> invoke(v:T) = object: FOO() { override fun doit() { println("Int: ${v}") } }
    }
}

fun main() {
  FOO("10").doit()
  FOO(10).doit()
  FOO(false).doit()
  FOO(10.0).doit()
}

$ kotlinc Foo2.kt -include-runtime -d Foo2.jar
$ java -jar Foo2.jar
Any: 10
Int: 10
Any: false
Number: 10.0
```
