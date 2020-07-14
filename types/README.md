# Types


 "어떤 term t가 T 타입을 갖는다"

 - "t는 T 타입에 속한다"
 - "t는 T의 원소다"
 - "t는 정적으로 보았을 때 (동적인 실행 없이 보아도) T 타입의 값으로
   evaluate 된다"
 - `t: T`

## Formal Definitions
### Typing Relation

 어떤 표현식에 대해서, 타입 규칙을 만족하는 가장 작은 relation. 이때
 `t: T`를 만족하는 T가 존재하면 term t는 *typable* 또는 *well typed*
 라고 한다.

### Typing Derivation

 어떤 표현식에 대해서 적용 가능한 타입 규칙을 적용한 인스턴스의
 트리. 즉,

  - *Statement*: 프로그램의 타입을 정하는 formal assertions
  - *Typing Rules*: statement 사이의 implication
  - *Derivation*: 타입 규칙에 근거한 Deduction

## Safety = Progress + Preservation
### Safety (Soundness)
 Well-typed term은 절대로 "잘못 되지 않는다".

 - "잘못 되다": 어떤 term이 evaluation rule에 의해 normal form(더이상
   적용할 evaluation rule이 없는 경우)에 도달했지만, 실제로는 아무런
   값도 아닌 경우. 즉, 대부분의 경우는 (타입 미스매치로 인한) 런타임
   에러를 뜻한다.


#### Progress
 Well-typed term은 항상 둘 중 하나다: 이미 어떤 값이거나, 아니면 타입
 규칙에 의해서 한 스텝 더 갈 수 있거나.


#### Preservation
 Well-typed term이 한 스텝을 더 가면, 그 결과 역시 well-typed 이다.


 대부분의 경우 well-typed term이 한 스텝 더 가면 그 결과 또한
 well-typed일 뿐만 아니라 정확하게 같은 타입을 갖는다. 하지만 몇몇
 시스템의 경우는 규칙을 적용하면서 타입이 변하기도 하는데, 예를 들면
 서브타입 시스템은 좀더 작은 타입이 되기도 한다.
