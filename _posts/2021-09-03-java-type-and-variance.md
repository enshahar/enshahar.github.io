---
layout:     post
title:      "[프로그래밍기초]정적 타입 지정과 변성"
date:       2021-09-03 17:26:00
summary:    "정적 타입 지정과 변성에 대해 정리"
categories: Java
---

# 정적 타입 지정과 변성

## 1. 정적 타입 지정이 유용한 이유

벤자민 피어스(Benjamin C. Pierce, "타입과 프로그래밍 언어"라는 책의 저자)는 "타입 시스템은 프로그램 각 부분이 만들어낼 수 있는 값의 종류를 나눔으로써 프로그램이 특정 오류 상황을 발생시키지 않는다는 점을 자동으로 검증하기 위한 문법적인 검증 방법"이라고 정의한다.

타입을 사용하면 함수의 정의역(domain)과 공변역(codomain)을 지정할 수 있다. 실제 프로그래밍에서는 함수(넓게 보면 연산자나 객체나 클래스의 메서드도 함수라 할 수 있고, 람다 산법(lambda calculus)을 활용하면 모든 프로그램 요소가 실제로는 함수 정의와 함수 적용의 설탕문법에 지나지 않는다는 사실을 보일 수도 있다. 다만 이 글은 자바 개발자를 대상으로 하므로 수학적인 함수가 아닌 자바 코드상의 함수에 대해서는 함수라는 용어 대신 메서드라는 용어를 쓸 것이다.)가 파라미터로 받을 수 있는 타입과 결과로 내놓는 타입을 지정함으로써, 프로그램 내에서 각 이름(변수)이 취할 수 있는 값이 어떻게 변화하는지를 대략적으로 **컴파일 시간에** 알아내기 위해 타입이 유용하게 쓰인다. 실제 프로그램은 항상 함수를 통해 값을 변화시키는 과정의 연속으로 이뤄지기 때문에, 함수의 정의역과 공변역을 타입으로 지정하고 그에 따라 컴파일 시간에 프로그램 내의 값 변화를 대략적으로 추론할 수 있다는 이야기는 프로그램 전체의 값 변화를 컴파일 시점에 대략적으로 추론할 수 있다는 뜻이기도 하다.

이런 타입 시스템을 사용하면 컴파일러는 정적(컴파일 시점)으로 프로그램이 건전성(soundness)을 판단할 수 있다. 건전성이라는 말은 프로그램의 각 요소의 타입 정보를 만족시킬 수 없는 프로그램은 컴파일에 실패하며, 컴파일에 성공한 프로그램은 항상 프로그램의 타입을 만족하는 값만 생성해 낸다는 뜻이다. 

일반적으로 모든 건전하지 못한 프로그램은 타입 시스템을 통과할 수 없지만, 건전한 프로그램 중에도 타입 시스템을 통과할 수 없는 프로그램이 있다(즉, 잘못된 값을 생성하지 않는 건전한 프로그램 중에도 컴파일러의 타입 검사를 통과하지 못하는 경우가 있다). 이런 격차(타입 시스템이 걸러내는 프로그램의 집합과 건전한 프로그램의 집합 사이의 차이)를 줄이려면 타입 시스템의 표현력을 더 좋게 만들어야 한다. 예를 들어 값에 의존적인 타입(value dependent type)을 사용하면 타입 검증을 통해 프로그램이 만들어낼 수 있는 값의 종류를 컴파일 시점에 추론할 수 있다. 현재도 학계에서는 이런 분야에 대한 연구가 진행중이다. 

### 1.1 타입의 계층 관계와 리스코프 치환

일반적으로 어떤 타입에 속한 값은 다른 타입에 속한 값 대신에 쓰일 수 없다. 하지만 모든 타입이 서로 아무 관계도 없는 타입이라면 프로그래밍이 너무 귀찮아진다. 필요할 때마다 타입을 변환해줘야 하고, 더 일반적인 패턴으로 정의할 수 있는 함수를 개별 타입을 고려한 모든 조합에 대해 정의해야 하기 때문이다. 

이런 제약을 없애기 위해 타입에도 계층을 둘 수 있다. 어떤 타입 A가 다른 타입 B의 상위타입(super type)이면 B 타입에 속하는 값을 모두 A 타입의 값으로 간주할 수 있다는 뜻이다. 이때 A타입의 값이 필요한 장소에 B 타입의 값을 넣어도 아무 문제 없이 프로그램이 실행될 수 있어야 한다. 한편, 상위타입의 반댓말은 하위타입(sub type)이다.

다른 말로 하면 하위타입은 상위타입을 만족하면서 더 많은 제약을 가한 타입을 뜻한다. 예를 들어 정수는 실수에 속하지만 소수점 이하가 존재하지 않는 실수를 뜻한다. 따라서 실수가 필요한 위치에 정수를 사용해도 (물론 2진 인코딩 방법이 달라서 변환이 필요하다고 주장하는 독자도 있겠지만) 프로그램에서는 아무 문제가 없다. 

반면, 하위 타입의 값이 필요한 위치에 상위 타입의 값을 사용하면 하위 타입이 강제하는 제약 사항을 만족시키지 못할 수도 있기 때문에 프로그램이 오작동할 수 있다.


### 1.2 객체지향의 상속 관계와 타입 시스템

객체지향에서 각 클래스는 서로 다른 타입으로 취급된다. 하지만 객체에 따라서는 다른 클래스에 정의된 모든 연산과 데이터를 제공할 수 있는 객체도 있다. 이런 객체 사이에는 상하위타입 관계가 성립하는게 바람직하다. 다행이 객체지향에서는 상속관계라는 관계가 존재하며, 우연히도(물론 실제로는 상속 관계에 있는 두 객체가 가질 수 있는 값과 연산의 정의 방식에 의해 필연적으로 정해지는 특성이기는 한데) 상속관계에 속하는 두 클래스에 속하는 객체 사이에는 **리스코프 치환(Liskov substitution) 원리**가 성립하는데, 이 원리는 정확히 **상하위타입 관계**의 두 타입에 속하는 값들 사이의 관계와 동일하다. 따라서, 객체지향 프로그래미 언어에서 상속관계는 구현을 공유하는 수단인 동시에 상하위 타입 관계를 명시하는 수단이기도 하다.

## 2. 파라미터화와 제네릭스

### 2.1 메서드와 타입 파라미터화, 제네릭스

메서드는 어떤 타입의 값을 파라미터로 받아 다른 타입의 값을 반환한다. 이를 더 일반화하면 어떤 타입을 파라미터로 받아서, 메서드를 만들어내는 메서드를 생각할 수도 있다. 타입을 파라미터로 받기 위해서는 `<T>`처럼 `<>` 사이에 타입 파라미터의 이름을 지정한다. 이런 메서드를 제네릭 메서드라고 부른다.

```
<T> T generic_function(int v) { ... } // T타입을 받아서, "int 타입의 값을 받아 T 타입의 값을 반환하는 메서드"를 반환하는 메서드
<T> int generic_function(T v) { ... } // T타입을 받아서, "T 타입의 값을 받아 int 타입의 값을 반환하는 메서드"를 반환하는 메서드
<T> T generic_function(T v) { ... }   // T타입을 받아서, "T 타입의 값을 받아 T 타입의 값을 반환하는 메서드"를 반환하는 메서드
```

한편 클래스에 타입 파라미터를 지정할 수도 있다. 이렇게 정의된 제네릭 클래스는 타입 파라미터를 메서드의 반환 타입이나 파라미터 타입, 필드의 타입 등에 사용할 수 있다. 자바에서 제네릭 클래스의 타입 파라미터는 메서드(또는 메서드)의 경우와 달리 클래스 이름 뒤에 온다는 점에 유의하라.

```
class Cell<T> {
  private T value;
  Cell(T v) { value=v; }
  T get() { return value; }
  void set(T v) { value=v; }
}
```

한편, 제네릭 메서드와 제네릭 클래스를 모두 타입을 인자로 받아서 다른 타입을 반환하는 메서드라고 생각할 수도 있다. 

### 2.2 제네릭 타입 사이의 상하위타입 관계

어떤 두 클래스 `A`와 `B`가 있다면 이 둘 사이가 상속 사슬로 연결된 경우 `A`와 `B`의 상하위타입 관계를 쉽게 알 수 있다. 예를 들면, `class Student`가 `class Human`을 상속하고, `class Human`이 `class Animal`을 상속한다면, `Student`는 `Human`의 하위타입인 동시에 `Animal`의 하위타입이 된다. 앞에서 말했던 것처럼 이런 타입들 사이의 관계는 리스코프 치환 원리를 만족하는지 살펴봄으로써 확인할 수 있다. 다음을 보자.

```
class Student extends Human { 
  void 땡땡이치기() { System.out.println("학교를 가지 않고 만화가게에 갑니다"); }
}
class Human extends Animal {
  void 말하기() { System.out.println("사느냐 죽느냐 그것이 문제로다!"); }
}
class Animal {
  void 움직이기() { System.out.println("이동합니다"); }
}
```

이런 세 클래스가 있을 때, `Animal` 타입의 변수에 `Student` 타입의 값을 대입해도 안전하게 `Animal`처럼 사용할 수 있다. 즉, `Student` 타입의 값은 `Animal` 타입의 하위 타입으로, 리스코프 치환 원리를 만족한다. 

```
Animal animal = new Student();
animal.움직이기();    // 이동합니다 출력됨
animal.땡땡이치기();  // 컴파일 오류
```

반면 (타입 체크를 무시하고 변수 대입이 가능하다고 가정할 때) `Student` 타입의 변수에 `Animal` 타입의 값을 담으면 문제가 생기는 경우가 있다.

```
Student student = new Animal();  // 컴파일이 된다고 가정
student.움직이기();    // 이동합니다 출력됨
student.땡땡이치기();  // 컴파일은 되겠지만 실제로는 땡땡이치기() 메서드를 제공하지 않으므로 런타임 오류 발생
```

따라서 리스코프 치환 원리에 따라 생각해보면 `Animal` 타입은 `Student` 타입의 하위 타입이 아니다.

어떤 제네릭 타입 `G<T>`가 있다면 이 제네릭 타입의 타입 파라미터에 구체적인 참조 타입을 지정해 생성한 여러 타입 인스턴스가 있을 수 있다. 예를 들어 앞에서 정의한 `Cell<T>`의 경우, 이 셀에 어떤 타입의 값을 담느냐에 따라 `Cell<Integer>`, `Cell<Double>`, `Cell<Animal>` 등 다양한 타입 인스턴스가 존재할 수 있다. 이제 "이런 타입 인스턴스들 사이에는 상하위 타입 관계가 성립하지는 않을까?"라는 질문이 자연스럽게 떠오른다. 

예를 들어 다음과 같은 `ReadOnlyCell`이라는 제네릭 타입이 있다고 하자.

```
class ReadOnlyCell<T> {
    private T value;
    ReadOnlyCell(T v) { value = v; }
    T get() { return value; }
}
```

이 읽기 전용 셀에 `Animal`을 담을 수도 있고, `Integer`를 담을 수도 있다.

```
var animalCell = new ReadOnlyCell<Animal>(new Animal());
var integerCell = new ReadOnlyCell<Integer>(1);  // 자동박싱

animalCell.get();
integerCell.get();
```

`ReadOnlyCell<Integer>` 타입의 변수에 `ReadOnlyCell<Animal>` 타입의 객체를 넣어도 문제가 없을까? 타입 검사에 성공한다고 가정하면 어떤 일이 벌어질지 생각해보자.

```
ReadOnlyCell<Intger> integerCell = new ReadOnlyCell<Animal>(new Animal());  // 컴파일이 된다고 가정하자
var newValue = integerCell.get() + 3;  // 런타임 오류
```

이 코드의 두번째 줄에서 `integerCell.get()`은 정수를 반환해야 하지만, 실제 `integerCell`이 가리키고 있는 객체는 `Animal` 타입의 값을 반환하기 때문에 타입 변환이 불가능하다는 런타임 오류가 발생한다고 추론해볼 수 있다. 물론 이런 일이 있으면 안되므로 컴파일러는 처음부터 `integerCell`에 `ReadOnlyCell<Animal>` 타입의 객체를 넣지 못하게 타입 오류를 발생시켜준다. 애초  `Animal`과 `Integer`는 전혀 관계가 없는 타입이기 때문에 이런 처리가 타당하다.

그렇다면 타입 파라미터 사이에 상속관계(따라서 상하위 타입 관계)가 있는 경우에는 제네릭 타입의 타입 인스턴스 사이에 어떤 상하위 타입 관계가 존재할까? 이런 성질을 변성(variance)이라고 부른다.

앞에서 `animal`과 `student`에 대해 리스코프 치환 규칙이 성립하는지를 살펴봤던 것처럼, 여기서도 `ReadOnlyCell<Animal>` 타입의 객체와 `ReadOnlyCell<Student>` 타입의 객체 사이에 어떤 치환 규칙이 성립하는지 살펴보자. 이를 위해 서로 다른 제네릭 타입 인스턴스 사이의 대입이 성공한다고 가정하고, 어떤 일이 생기는지 생각해보자.

```
ReadOnlyCell<Animal> animalCell = new ReadOnlyCell<Student>(new Student()); // 컴파일이 된다고 가정
Animal animal = animalCell.get();      // animal에 Student 객체가 대입됨
animal.움직이기();                      // 문제 없음

ReadOnlyCell<Student> studentCell = new ReadOnlyCell<Animal>(new Animal()); // 컴파일이 된다고 가정
Student student = studentCell.get();      // student에 Animal 객체가 대입됨
student.움직이기();                        // 문제 없음
student.땡땡이치기();                      // 런타임 오류(Animal 객체는 땡땡이치기()를 제공하지 않음)
```

여기서 `ReadOnlyCell<Animal>` 타입의 값이 필요한 위치에 `ReadOnlyCell<Student>` 타입의 값을 사용해도 문제가 없다는 사실을 알 수 있다. 따라서 리스코프 치환 원칙에 따라 `ReadOnlyCell<Animal>` 타입은 `ReadOnlyCell<Student>` 타입의 상위타입이다. 이런식으로 제네릭 타입 `G<T>`가 있고, `A`가 `B`의 상위타입일 때 제네릭 타입의 인스턴스인 `G<A>`가 `G<B>`의 상위타입이 되는 경우, 제네릭 타입 `G<T>`가 타입 파라미터 `T`에 대해 공변적(covariant)이라고 말한다. 공변적이라는 말은 제네릭 타입의 상하위타입 관계가 타입 파라미터의 상하위타입 관계와 같은 방향으로 변한다는 뜻이다.

모든 제네릭 타입이 타입 파라미터에 대해 공변적일까? 그렇지는 않다. 값을 넣기만 하고 뺄 수는 없는 쓰기 전용 셀 `WriteOnlyCell`을 정의해보자.

```
class WriteOnlyCell<T> {
    private T value;
    WriteOnlyCell(T v) { value = v; }
    T set(T v) { value=v; }
}
```

이제 이 제네릭 클래스에 때해 어떤 리스코프 치환 규칙이 적용되는지 살펴보자.

```
WriteOnlyCell<Animal> animalCell = new WriteOnlyCell<Student>(new Student()); // 컴파일이 된다고 가정
animalCell.set(new Student());    // 셀에 Student 객체 대입. 문제 없음
animalCell.set(new Animal());     // 셀에 Animal 객체 대입. 하지만 Cell<Student>의 set 메서드는 실제로는 Student타입 또는 하위 타입의 값만 저장할 수 있음

WriteOnlyCell<Student> studentCell = new WriteOnlyCell<Animal>(new Animal()); // 컴파일이 된다고 가정
studentCell.set(new Animal());     // 셀에 Animal 객체 대입. 문제 없음
studentCell.set(new Student());    // 셀에 Animal 객체 대입. 문제 없음(Student가 Animal이기도 하므로 대입 가능)
```

앞의 `ReadOnlyCell`과 반대로 `WriteOnlyCell<Student>`의 위치에 `WriteOnlyCell<Animal>`을 써도 문제가 없었고 반대방향으로 객체를 대체하면 문제가 생겼다. 따라서  `WriteOnlyCell<Student>`는 `WriteOnlyCell<Animal>`의 상위타입이다. 이처럼 `A`가 `B`의 상위타입일 때 `G<B>`가 `G<A>`의 상위타입인 경우를 반공변(contravariant)이라고 부른다. 반공변은 제네릭 타입의 상하위타입 관계가 타입 파라미터의 상하위타입 관계와 반대방향으로 변한다는 뜻이다.

한편, `ReadOnlyCell`과 `WriteOnlyCell`를 합친 `Cell`의 경우, `Cell<Animal>`를 대신 `Cell<Student>` 인스턴스를 사용하면 `set`에서 문제가 발생하고, `Cell<Student>`를 대신 `Cell<Animal>` 인스턴스를 사용하면 `get`에서 얻은 객체에서 문제가 생긴다. 따라서 `Cell`이라는 제네릭 타입은 타입 파라미터의 상하위타입 관계와 무관하게 언제나 아무 상하위타입 관계도 성립하지 않는다. 이런 경우를 무공변(invariant)이라고 부른다.

제네릭 메서드의 경우 어떨까? 앞에서 본 Student, Human, Animal 사이의 상하위타입관계가 제네릭 메서드에서 어떻게 성립하는지 생각해보자. 우선 반환 타입으로 타입 파라미터가 쓰이는 경우를 생각해보자. 

`Student`를 반환하는 메서드 위치에 `Animal`을 반환하는 메서드를 넣어도 될까? 그렇지 않다. `Student`를 반환하는 메서드를 사용하는 쪽에서는 메서드가 반환하는 `Student` 값을 활용할 것이다. 따라서 `Student`만의 메서드등을 활용할 수 있다. 만약 `Student`를 반환하는 메서드 대신 `Animal`을 반환하는 메서드를 넣는다면 이 메서드가 실제로는 `Animal`을 반환하기 때문에 문제가 생길 수 있다. 반면, `Animal`을 반환하는 메서드 위치에 `Student`를 반환하는 메서드를 넣는다고 해도 아무 문제가 없다. 따라서 제네릭 메서드에서 타입 파라미터가 반환 값 위치에 쓰이는 경우, 제네릭 메서드는 타입 파라미터에 대해 공변적이다.

반대로 `Student`를 인자로 받는 메서드가 필요한 위치에 `Animal`을 인자로 받는 메서드를 넣어도 될까? 그렇다. `Student`를 인자로 받는 메서드 위치에는 항상 `Student` 타입(또는 그 하위 타입)의 값만 전달된다는 점에 착안하자. 이 위치에 `Animal`을 인자로 받는 메서드를 넣으면 어떤 일이 생길까? `Animal`을 인자로 받는 메서드는 내부에서 `Animal`이 지원하는 연산만 사용할텐데, `Student` 객체는 항상 상위 타입인 `Animal` 타입 객체의 연산을 모두 지원한다. 하지만 `Animal`을 인자로 받는 메서드가 필요한 위치에 `Student`를 인자로 받는 메서드를 넣는다면, `Student`를 인자로 받는 메서드의 인자로 `Animal` 객체가 전달된다. 하지만 이 메서드 내부는 `Student`를 인자로 받는 메서드이기 때문에 `Student`만 지원하는 연산도 호출할 수 있기 때문에, `Animal` 객체에서 문제가 발생한다. 따라서 제네릭 메서드의 타입 파라미터가 메서드의 파라미터 위치에 사용되는 경우, 제네릭 메서드 타입은 타입 파라미터의 타입에 대해 반공변적이다. 

제네릭 메서드의 타입 파라미터가 결과 타입인 경우에 대한 공변성은 비교적 쉽게 이해할 수 있겠지만, 타입 파라미터가 파라미터 타입으로 쓰이는 경우의 반공변성은 설명만으로는 이해하기 어려울 수도 있다. 하지만 다음 코드를 보면 쉽게 이해할 수 있다.

```
// 모두 컴파일은 제대로 된다고 가정해보자
// 람다의 타입을 (파라미터타입)->반환타입 처럼 지정할 수 있다고 가정
var lambda1: (Student)->Void = (Student st)->st.땡땡이치기();
var lambda2: (Animal)->Void = (Animal a)->a.움직이기();

(Student)->Void 땡땡이 = lamdba2;   // (Student)->Void 타입의 변수에 (Animal)->Void 타입의 값을 지정
땡땡이(new Student());              // 새 Student 객체를 만들어 땡땡이에 넘김. lambda2는 움직이기()를 실행하므로 정상 동작

(Animal)->Void 움직임 = lambda1;   // (Animal)->Void 타입의 변수에 (Student)->Void 타입의 값을 지정
움직임(new Animal());              // 새 Animal 객체를 만들어 땡땡이에 넘김. lambda1는 땡땡이치기()를 실행하므로 오류 발생
```

### 2.3 in위치, out위치, (자바에서 쓸 수 없는) 선언 지점 변성

제네릭 메서드나 제네릭 클래스의 타입 파라미터가 반공변적인 위치에 쓰일 때 우리는 이 타입 파라미터가 `in`위치에 쓰인다고 말한다. 메서드의 파라미터로 전달되는 경우가 반공변적인 위치다. 반대로 타입 파라미터가 공변적인 위치에 쓰이면 이 타입 파라미터가 `out` 위치에 있다고 말한다. 언어에 따라서는 클래스를 정의할 때 타입 파라미터가 어떤 위치에 쓰일지를 지정할 수 있게 해주는 경우가 있다. 이런 변성 지정 방식을 **선언 지점 변성(declaration site variance)**이라고 부른다. 예를 들어 코틀린의 경우 앞에서 본 `ReadOnlyCell`과 `WriteOnlyCell`의 타입 파라미터 앞에 각각 `out`과 `in`을 붙여서 각 타입 파라미터가 공변적, 반공변적임을 표현할 수 있다.

```
// 코틀린 코드
class ReadOnlyCell<out T>(v: T) {
  private val value: T = v
  fun get():T = value
}

class WriteOnlyCell<in T>(v: T) {
  private var value: T = v
  fun set(v:T) { value=v }
}
```

이런 언어에서는 타입 파라미터에 변성이 표현되어 있기 때문에, 제네릭 타입 인스턴스 사이의 공변성을 컴파일러가 알 수 있다. 또, `in`으로 선언된 타입 파라미터를 잘못된 위치(예: 메서드 반환 타입)에 사용하거나, `out`으로 선언된 타입 파라미터를 잘못된 위치(예: 메서드 파라미터 타입)에 사용하면 컴파일러가 오류를 표시할 수 있다. 따라서 이런 언어에서는 개발자가 표시한 타입 파라미터의 공변성을 제네릭 메서드나 제네릭 클래스 정의가 위배하는 경우 컴파일 오류를 표시해준다.

```
// 코틀린 코드
class Cell1<out T>(v: T) {
  private var value: T = v
  fun get():T = value
  // error: type parameter T is declared as 'out' but occurs in 'in' position in type T
  fun set(v:T) { value=v }   
}

class Cell2<in T>(v: T) {
  private var value: T = v
  // error: type parameter T is declared as 'in' but occurs in 'out' position in type T
  fun get():T = value
  fun set(v:T) { value=v }
}
```

따라서 `Cell`과 같이 타입 파라미터가 공변적 위치와 반공변적 위치에 동시에 사용되는 경우에는 어쩔 수 없이 공변성이 없이 타입 파라미터를 지정해야 하며 이런 경우 타입 파라미터와 제네릭 타입은 무공변이 된다.

코틀린 등의 언어와 달리 자바에서 모든 제네릭 타입의 인스턴스는 서로 무공변이다. 자바에서 `Number`는 `Integer`의 상위 타입이지만, `List<Integer>`와 `List<Numnber>`는 서로 상하위 타입 관계가 없다. 따라서 `List<Integer>` 대신 `List<Number>`를 사용하거나 `List<Number>` 대신 `List<Integer>`를 사용할 수 없다. 


### 2.4 자바에서 쓸 수 있는 사용 지점 변성

자바 제네릭 클래스나 메서드의 타임 파라미터는 무공변이다. 그렇지만 실제 제네릭 타입의 공변성이나 반공변성을 활용할 수 있으면 좋은 경우가 자주 있다. 예를 들어, `Animal`이 들어있는 `List`를 받아서 모든 원소를 이동시키는 메서드가 있다고 하자.

```
void moveAll(List<Animal> list) { 
  for(Animal a:list) {
    a.움직이기();
  }
}
```

`moveAll` 메서드를 작성한 우리는 `list`의 모든 원소에 대해 `Animal`의 메서드만 사용하기 때문에 `List`안에 `Animal`의 하위타입이 들어가도 안전하다는 사실을 안다. 하지만 자바의 무공변성으로 인해 이 `moveAll` 메서드에 `Student`의 리스트나 `Human`의 리스트를 전달하면 정상적으로 컴파일이 되지 않는다. 

```
var list = new LinkedList<Animal>();
list.add(new Animal());
moveAll(list);

var list2 = new LinkedList<Student>();
list2.add(new Student());
moveAll(list2); // Error: incompatible types: java.util.LinkedList<Student> cannot be converted to java.util.List<Animal>
```

가능하다면 이런 제약을 없애고 싶다. 이럴때 `? extends Animal`로 리스트의 타입을 제약하면 타입 파라미터가 `Animal`의 하위 타입인 리스트만 받아들이는 함수를 만들 수 있다.

```
void moveAll2(List<? extends Animal> list) { 
  for(Animal a:list) {
    a.움직이기();
  }
}
moveAll2(list2);  // "이동합니다" 3번 표시
```

이런 식의 변성 지정을 **사용 지점 변성(use site variance)**이라고 말한다. 메서드 파라미터 뿐 아니라 변수 선언에서도 변성을 지정할 수 있다. 이번에는 좀 더 다른 예를 보자. 어떤 제네릭 클래스에서 타입 파라미터가 공변적인 위치와 반공변적인 위치에서 모두 쓰이면 이 제네릭 클래스의 타입 파라미터는 무공변일 수 밖에 없음을 앞의 `Cell` 예제에서 살펴봤다. 하지만 프로그래머가 이 제네릭 클래스의 공변적인 기능만 사용하거나 반공변적인 기능을 사용한다는 점이 분명한 경우, 사용 지점 변성을 통해 이런 사실을 알릴 수 있다. 

```
class Cell<T> {
  private T value;
  Cell(T v) { value=v; }
  T get() { return value; }
  void set(T v) { value=v; }
}

Cell<? extends Animal> readOnlyCell = new Cell<Student>(new Student()); // 공변성
readOnlyCell.get()  // 정상작동
readOnlyCell.set(new Animal())  // Error: incompatible types: Animal cannot be converted to capture#1 of ? extends Animal
```

타입 파라미터를 메서드의 반환 타입으로 사용하는 `T get()`의 경우 아무 문제 없이 잘 작동하지만, 반공변적인 위치에 타입 파라미터가 쓰인 `set()`에 값을 대입하려고 하면 컴파일 오류가 발생해 문제를 미리 방지해준다. 

반대로 `Cell`을 쓰기만 하고 싶을 때는 `<? super Student>` 형태의 타입 인자를 사용해 반공변성임을 알릴 수 있다.

```
Cell<? super Student> writeOnlyCell = new Cell<Animal>(new Animal()); // 반공변성
writeOnlyCell.set(new Animal());   // 정상작동
writeOnlyCell.set(new Student());  // 정상작동
writeOnlyCell.get();  // Error: incompatible types: Animal cannot be converted to capture#8 of ? super Student
```
