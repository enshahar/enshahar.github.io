---
layout:     post
title:      "(FP,용어,스칼라) 부분함수"
date:       2017-11-14 15:08:00
summary:    "부분함수라는 용어를 소개하고, 스칼라에서 부분함수를 어떻게 정의하고 활용하는지 소개한다"
categories: FP terms scala
---

스칼라를 처음 시작하는 사람들은 다양한 용어에 곤란을 겪곤 합니다. 그래서 용어 중심으로 하나씩 스칼라 내용을 풀어가는 작업을 해보려 합니다. 그 첫번째는 부분함수(partial function)입니다.

제 생각에는 스칼라에서 `PartialFunction`이 커링이나 `for` 컴프리핸션 정도로 중요한 개념은 아닙니다. 개념이 생소할 뿐이죠. 필요성보다는 그게 뭐고, 어떤 용례가 있는지 이해하고 넘어가면 될 것 같습니다.

## 부분함수의 정의

수학적으로 부분함수는 정의역 중 일부분에 대해서만 함수 값이 정의되는 함수를 뜻합니다. 예를 들어 `X = {1,2,3,4}`, `Y = {A,B,C}`가 있을 때, `(1,A),(2,B),(3,C)`와 같은 식으로 `X`의 모든 원소에 대한 대응이 정의되지 않는 경우를 부분함수(또는 부분정의함수)라고 합니다. 영어로는 partial function이나 partially defined function이라고 부릅니다. 수학에선 함수를 더 일반화한 것이 부분함수라고 할 수 있습니다.

스칼라에서도 `PartialFunction`은 함수입니다. 따라서, `Function`을 대신할 수 있습니다. 함수를 호출할 수 있는 곳에서는 `PartialFunction`을 대신 사용할 수 있겠죠.

다만, `PartialFunction`은 `isDefinedAt(value)`을 제공해야 합니다. 이 메서드는 `value`에 대해 함수값이 정의돼 있으면 참, 그렇지 않으면 거짓을 반환합니다.

### 부분함수가 필요한 이유

어떤 함수를 호출하면 결과가 정상적으로 나오지 않을 때가 있다면, 그런 경우를 어떻게 처리해야 할까요? 몇 가지 방법이 있겠죠. 대충 생각나는걸 적어보면…

1. 예외를 던진다. 굳이 자세한 설명이 필요 없겠죠?
2. 정상적이지 않은 경우를 표현하는 다른 반환값을 사용한다. 예를 들어 스칼라의 `Option`이나 `Either` 등을 사용하면 정상적인 값과 그렇지 않은 값을 처리할 수 있습니다. 자바라면 `null`을 던지는 것도 한가지 방법이긴 하겠죠(그렇다고 그렇게 해도 된다는 이야기는 아님).

자바의 경우 메서드 뒤에 `throws`를 붙여서 꼭 발생할 수 있는 예외를 명시해야 하므로 어떤 함수가 예외를 발생시킬지 알 수 있습니다. 하지만 스칼라에서는 예외를 반드시 명시할 필요가 없으므로(`@throws` 애노테이션이 있지만 쓰기 싫으면 안써도 되죠), 2번처럼 `Option`이나 `Either`, `Try` 등을 활용할 것을 권장합니다. 함수의 반환 타입만 봐도 "아.. 이 함수는 예외적인 경우에 다른 값을 내놓는 구나"하고 감이 오니까요.

반대로 어떤 함수가 특정한 값에 대해서만 작동하거나, 특정한 값에 대해서만 올바른 결과값을 반환할 것을 보장한다면 이를 어떻게 표현할 수 있을까요? 이때는 다음과 같은 방법이 있겠죠.

1. `InvalidArgumentException`을 던진다.
2. 입력오류를 표현하는 결과값을 반환한다.
3. `PartialFunction`으로 함수를 만들고, `isDefinedAt`으로 먼저 함수가 정상적인 값을 반환할지를 물어보고, `isDefinedAt`이 참을 반환하는 경우에만 함수를 호출한다.

1번 방식은 자바식 방법이겠죠. 2번은 어떤 언어에서건 별로 바람직한 방법은 아닐겁니다. 미리 오류를 표현하는 결과값이 뭔지 알야아 하고, 함수를 호출한 다음 결과값을 매번 체크해야 하고, 예외적 상황과 프로그램 메인 흐름이 분리되지 않으니까요. 3번은 `PartialFunction`라는 함수의 타입만 보고, 그 함수가 정해진 몇가지 값에 대해서만 정의된 함수라는 것을 알 수 있습니다.

이렇게 표현했을 때 얻을 수 있는 이점 한가지는, `PartialFunction`끼리 조합해서 새로운 `PartialFunction`을 만들 수 있다는 점입니다. `PartialFunction`에 있는 다음 메서드를 봅시다.

- `def andThen[C](k: (B) ⇒ C): PartialFunction[A, C]`
- `def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]): PartialFunction[A1, B1]`

`andThen`은 부분함수가 정의된 경우에, 그 결과에 다른 함수를 적용해서 반환합니다. 반면, `orElse`는 인자로 들어온 값에 대해 부분함수가 정의된 경우에는 그냥 결과값을 반환하고, 그렇지 않은 경우에는 다른 부분함수를 호출합니다. 그 부분함수가 원래의 인자에 대해 정의돼 있다면 그 결과값이 반환되겠죠. `andThen`은 `isDefinedAt()`이 참인 범위가 확장되지 않고 그대로 유지되지만, `orElse`는 두 부분함수의 정의역의 합집합이 새로운 정의역이 되겠죠.

### 부분함수가 지켜야 할 사항

`PartialFunction`은 `isDefinedAt`을 제공하며, `isDefinedAt`이 참을 반환하는 경우에는 정상적인 동작과 반환값을 보장한다는 의미가 있습니다. 호출하는 쪽에서는 먼저 `isDefinedAt`을 호출해서 함수가 정상 작동할지를 확인할 수 있기 때문에 정상작동할거라는 확신을 가지고 함수를 호출할 수 있습니다. 물론 컴파일러가 그런 약속을 강제하지는 않기 때문에, 부분함수를 작성하는 사람이 조심해 작성해야 하고, 쓰는 사람도 가급적 `isDefinedAt()`을 호출해 호출시 제대로된 출력이 나올지 미리 검토를 해야 합니다.

## 예

### 간단한 예

예를 봅시다.

```scala
// 72법칙에 따라 복리계산시 2배 되는 기간을 계산한다.
val doublePeriod = new PartialFunction[Int, Int] {
  def apply(d: Int) = 72 / d
  def isDefinedAt(d: Int) = d > 0 && d < 40 // 너무 이율이 크면 72법칙이 맞지 않는다.
}
```

이런 경우...

```scala
scala> doublePeriod(10)
res0: Int = 7

scala> doublePeriod(0)
java.lang.ArithmeticException: / by zero
  at $anon$1.apply$mcII$sp(<console>:12)
  ... 32 elided

scala> doublePeriod(50)
res2: Int = 1

scala> doublePeriod.isDefinedAt(50)
res3: Boolean = false

scala> doublePeriod.isDefinedAt(0)
res4: Boolean = false
```

보시면 아시겠지만, `isDefinedAt()`이 거짓이라고 해서 해당 함수를 호출하지 못하는 것도 아니고, 예외가 발생하지 않는 것도 아닙니다. 그러니까 호출하는 쪽에서 신경을 써줘야 합니다.

`andThen`과 `orElse`의 예를 봅시다.

```scala
val oddFt = new PartialFunction[Int, Int] {
  def apply(x: Int) = x + 1
  def isDefinedAt(x: Int) = x%2 == 1
}

val evenFt = new PartialFunction[Int, Int] {
  def apply(x: Int) = x * 2 + 1
  def isDefinedAt(x: Int) = x%2 == 0
} 
```

이제 이 둘을 가지고 실험합시다.

```scala
scala> val range = 1 to 10
range: scala.collection.immutable.Range.Inclusive = Range(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)


scala> range.map(oddFt.isDefinedAt)
res5: scala.collection.immutable.IndexedSeq[Boolean] = Vector(true, false, true, false, true, false, true, false, true, false)

scala> range.map(evenFt.isDefinedAt)
res6: scala.collection.immutable.IndexedSeq[Boolean] = Vector(false, true, false, true, false, true, false, true, false, true)

scala> oddFt(1)
res9: Int = 2

scala> evenFt(2)
res10: Int = 5

scala> val composed = oddFt andThen evenFt
composed: PartialFunction[Int,Int] = <function1>

scala> composed(1) = 5

scala> val merged = oddFt orElse evenFt
merged: PartialFunction[Int,Int] = <function1>

scala> merged(1)
res16: Int = 2

scala> merged(2)
res17: Int = 5

scala> range.map(merged.isDefinedAt)
res18: scala.collection.immutable.IndexedSeq[Boolean] = Vector(true, true, true, true, true, true, true, true, true, true)
```

## 스칼라에서 사용중인 부분함수의 예

### 액터의 receive

스칼라에서 가장 `PartialFunction`의 예로 유명한 것은 아마도 액터의 `receive`일 것입니다. 액터는 임의의 메시지를 받을 수 있기 때문에, 액터시스템에서는 어떤 액터가 메시지를 처리할지 아닐지를 미리 알면 좋습니다. 아카 액터의 API스칼라독을 보면:

- `type Receive = PartialFunction[Any, Unit]`
- `abstract def receive: Actor.Receive` : 최초 액터의 행동을 결정한다. 액터의 동작 로직이 들어있는 부분 함수여야만 한다.

여기서 `receive`의 타입은 `PartialFunction[Any,Unit]`입니다. 따라서, 액터 시스템은 어떤 액터가 메시지를 처리할 수 있는지 여부를 `receive`의 `isDefinedAt()`을 호출해서 알아낼 수 있습니다.

### 파서 컴비네이터의 `^?`

스칼라 파서 컴비네이터의 `Parser`를 보면 다음과 같은 메서드가 있습니다.

- `def ^?[U](f: PartialFunction[T, U]): Parser[U]` : 이 메서드는 파서가 파싱에 성공하면 `f`를 적용해주되, `f`이 파싱 결과값에 대해 정의돼 있지 않거나, 파서가 실패했다면 오류 메시지를 표시하고 파싱을 중단하겠죠.
- `def ^?[U](f: PartialFunction[T, U], error: (T) ⇒ String): Parser[U]` : 이 메서드는 파서가 파싱에 성공하면 `f`를 적용해주고, `f`가 파싱 결과값에 대해 정의돼 있지 않으면 `error`에 파싱 결과를 넘겨서 오류 메시지를 가져와 표시하고, 파서가 실패했다면 오류 메시지를 표시하고 파싱을 중단하겠죠.

## 부분함수를 정의하는 여러가지 방법

앞에서 부분함수를 직접 `PartialFunction`을 사용해 무명 클래스로 구현한 것을 봤습니다. 당연히 무명 클래스가 아닌 일반 클래스로 `PartialFunction`을 상속하거나 믹스인해서도 구현할 수 있겠죠. 이에 대해서는 따로 설명하지 않겠습니다. 이외에도 몇가지 유용한 방법이 있습니다.

### `match`문을 활용하는 방법

다음 프로그램을 봅시다.

```scala
// match.scala라는 이름으로 저장한다.
object MatchTest {
  def main(args: Array[String]):Unit = {
    val x = 1
    val y = (x:Int) => x match {
        case 1 => "One"
        case 2 => "Two"
        case 3 => "Three"
      }
    println(y(x))
    // 위 y와 완전히 같은 람다지만 타입만 다르게 선언
    val z:PartialFunction[Int,String] = (x:Int) => x match {
        case 1 => "One"
        case 2 => "Two"
        case 3 => "Three"
      }
    println(z.isDefinedAt(x))
  }
}
```

이 프로그램을 `scalac -Xprint:all .\match.scala`로 컴파일해보면 다음과 같습니다.

```scala
[[syntax trees at end of                    parser]] // match.scala
package <empty> {
  object MatchTest extends scala.AnyRef {
    def <init>() = {
      super.<init>();
      ()
    };
    def main(args: Array[String]): Unit = {
      val x = 1;
      val y = ((x: Int) => x match {
        case 1 => "One"
        case 2 => "Two"
        case 3 => "Three"
      });
      println(y(x));
      val z: PartialFunction[Int, String] = ((x: Int) => x match {
        case 1 => "One"
        case 2 => "Two"
        case 3 => "Three"
      });
      println(z.isDefinedAt(x))
    }
  }
}

[[syntax trees at end of                     namer]] // match.scala: tree is unchanged since parser
[[syntax trees at end of            packageobjects]] // match.scala: tree is unchanged since parser
[[syntax trees at end of                     typer]] // match.scala
package <empty> {
  object MatchTest extends scala.AnyRef {
    def <init>(): MatchTest.type = {
      MatchTest.super.<init>();
      ()
    };
    def main(args: Array[String]): Unit = {
      val x: Int = 1;
      val y: Int => String = ((x: Int) => x match {
        case 1 => "One"
        case 2 => "Two"
        case 3 => "Three"
      });
      scala.this.Predef.println(y.apply(x));
      val z: PartialFunction[Int,String] = ({
        @SerialVersionUID(value = 0) final <synthetic> class $anonfun extends scala.runtime.AbstractPartialFunction[Int,String] with Serializable {
          def <init>(): <$anon: Int => String> = {
            $anonfun.super.<init>();
            ()
          };
          final override def applyOrElse[A1 <: Int, B1 >: String](x: A1, default: A1 => B1): B1 = (x: A1 @unchecked) match {
            case 1 => "One"
            case 2 => "Two"
            case 3 => "Three"
            case (defaultCase$ @ _) => default.apply(x)
          };
          final def isDefinedAt(x: Int): Boolean = (x: Int @unchecked) match {
            case 1 => true
            case 2 => true
            case 3 => true
            case (defaultCase$ @ _) => false
          }
        };
        new $anonfun()
      }: PartialFunction[Int,String]);
      scala.this.Predef.println(z.isDefinedAt(x))
    }
  }
}

[[syntax trees at end of                    patmat]] // match.scala
package <empty> {
  object MatchTest extends scala.AnyRef {
    def <init>(): MatchTest.type = {
      MatchTest.super.<init>();
      ()
    };
    def main(args: Array[String]): Unit = {
      val x: Int = 1;
      val y: Int => String = ((x: Int) => {
        case <synthetic> val x1: Int = x;
        x1 match {
          case 1 => "One"
          case 2 => "Two"
          case 3 => "Three"
          case _ => throw new MatchError(x1)
        }
      });
      scala.this.Predef.println(y.apply(x));
      val z: PartialFunction[Int,String] = ({
        @SerialVersionUID(value = 0) final <synthetic> class $anonfun extends scala.runtime.AbstractPartialFunction[Int,String] with Serializable {
          def <init>(): <$anon: Int => String> = {
            $anonfun.super.<init>();
            ()
          };
          final override def applyOrElse[A1 <: Int, B1 >: String](x: A1, default: A1 => B1): B1 = {
            case <synthetic> val x1: A1 = (x: A1 @unchecked);
            case7(){
              if (1.==(x1))
                matchEnd6("One")
              else
                case8()
            };
            case8(){
              if (2.==(x1))
                matchEnd6("Two")
              else
                case9()
            };
            case9(){
              if (3.==(x1))
                matchEnd6("Three")
              else
                case10()
            };
            case10(){
              matchEnd6(default.apply(x))
            };
            matchEnd6(x: B1){
              x
            }
          };
          final def isDefinedAt(x: Int): Boolean = {
            case <synthetic> val x1: Int = (x: Int @unchecked);
            x1 match {
              case 1 => true
              case 2 => true
              case 3 => true
              case _ => false
            }
          }
        };
        new $anonfun()
      }: PartialFunction[Int,String]);
      scala.this.Predef.println(z.isDefinedAt(x))
    }
  }
}
... 이하 생략 ...
```

잘 보시면 `typer`가 타입체킹을 하면서 `PartialFunction`에 해당하는 경우에는 `isDefinedAt`과 `applyOrElse`를 자동으로 넣어준다는 것을 알 수 있습니다. 바로 이 부분이 `match`로 부분함수를 만드는 마법에 해당합니다. 우리가 사용한 형태 `val z: PartialFunction[Int, String] = ((x: Int) => x match { ... }`에서 `((x: Int) => x match` 부분의 `x`는 실제로는 전혀 프로그램의 의미에 영향을 끼치지 못하고, 단지 인자가 1개인 함수를 만들어주는 역할만을 합니다. (참고로 이런 형태로 `x`가 전혀 들어있지 않은 어떤 식 `expr`을 `(x:Type) => expr`과 같이 함수 형식으로 확장하는 것을 에타확장이라고 부릅니다.) 따라서, 스칼라는 불필요하게 프로그래머가 직접 에타 확장을 할 필요 없이 직접 `match`식의 `case` 부분만을 쓸 수 있게 허용합니다. 그에 따라 앞의 `z`를 다음과 같이 정의할 수도 있습니다.

```scala
val z2:PartialFunction[Int,String] = {
  case 1 => "One"
  case 2 => "Two"
  case 3 => "Three"
}
println(z2.isDefinedAt(x))
```

이 코드를 앞의 `match.scala`에 넣고 컴파일해보면 람다의 인자 부분을 제외하면 동일한 코드를 만들어준다는 사실을 알 수 있습니다. 이런 케이스 블럭은 `PartialFunction` 역할을 할 수 있고, `PartialFunction`은 `Function`이기도 하기 때문에, 함수가 필요한 곳에 람다 대신 케이스블럭을 넣을 수도 있습니다. 다만, 그런 경우에는 케이스 블럭에서 처리하지 않는 값에 대해서는 `MatchError`가 발생할 수 있음에 유의해야겠죠.

이런 이유로, 매치문을 (부분함수인 케이스 블럭을 사용해) 쉽게 고차함수에 넘길 수 있습니다.

```scala
scala> List(1,2,3) map { 
     | case 1 => "One"
     | case 2 => "Two"
     | case _ => "Other" }
res2: List[String] = List(One, Two, Other)
```

### 부분함수와 컬렉션

대표적인 스칼라 컬렉션 `List`, `Map`, `Array`에 대해 생각해 봅시다.

```scala
scala> val family = List("Frank","Kevin","Joshua")
family: List[String] = List(Frank, Kevin, Joshua)

scala> family(0)
res6: String = Frank

scala> family(3)
java.lang.IndexOutOfBoundsException: 3
  at scala.collection.LinearSeqOptimized$class.apply(LinearSeqOptimized.scala:65)
  at scala.collection.immutable.List.apply(List.scala:84)
  ... 32 elided

scala> val familyMap = Map( "dad" -> "Frank", "mum" -> "Joyce")
familyMap: scala.collection.immutable.Map[String,String] = Map(dad -> Frank, mum -> Joyce)

scala> familyMap("dad")
res8: String = Frank

scala> familyMap("mistress")
java.util.NoSuchElementException: key not found: mistress
  at scala.collection.MapLike$class.default(MapLike.scala:228)
  at scala.collection.AbstractMap.default(Map.scala:59)
  at scala.collection.MapLike$class.apply(MapLike.scala:141)
  at scala.collection.AbstractMap.apply(Map.scala:59)
  ... 32 elided

scala> val arr = Array(1,2,3,4,5)
arr: Array[Int] = Array(1, 2, 3, 4, 5)

scala> arr(0)
res10: Int = 1

scala> arr(-1)
java.lang.ArrayIndexOutOfBoundsException: -1
  ... 32 elided
```

`List`나 `Array`의 `i`번째 원소를 가져오는 연산(실제로는 `apply()` 메서드)의 특징은 뭘까요? 

- `i`가 `0` 이상이고 배열이나 리스트의 원소개수 미만이라면 해당 위치에 있는 원소를 반환한다.
- 그 이외의 경우에는 `IndexOutOfBoundsException`이나 `ArrayIndexOutOfBoundsException`를 던진다.

마찬가지로, `Map`의 경우는 다음과 같습니다. 
- 인자가 `Map`의 키 중 하나와 같으면, 그 키에 대응하는 값을 반환한다. 
- 인자와 일치하는 키가 없다면, `NoSuchElementException`을 반환한다.

어디서 많이 보던 패턴 아닌가요? 맞습니다. 그래서, 스칼라의 컬렉션들은 `PartialFunction`을 믹스인 합니다.

```scala
scala> family.isDefinedAt(100)
res12: Boolean = false

scala> familyMap.isDefinedAt("Hoho")
res13: Boolean = false

scala> arr.isDefinedAt(Int.MaxValue)
res14: Boolean = false
```

따라서, 스칼라에서 케이스 블럭을 사용하지 않고, `PartialFunction`을 정의하는 간단한 방법 중 하나는 `Map`을 만드는 것입니다. `List`나 `Array`는 `0`부터 시작하고, `1`씩 인덱스가 증가하는 연속된 범위에 대한 `PartialFunction[Int, 어떤타입]`만을 만들 수 있기 때문에 아무래도 쓸모가 좀 덜하겠죠.

```scala
scala> val y:PartialFunction[Int, String] = Map( 1->"One", 2->"Two" )
y: PartialFunction[Int,String] = Map(1 -> One, 2 -> Two)
```

부분함수(partial function)과 흔히 혼동하는 것이 부분 적용한 함수(partially applied function)입니다. 다음에 기회가 되면 그에 대해 다뤄볼까 합니다.