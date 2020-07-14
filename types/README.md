# Types


 "어떤 term t가 T 타입을 갖는다"

 - "t는 T 타입에 속한다"
 - "t는 T의 원소다"
 - "t는 정적으로 보았을 때 (동적인 실행 없이) T 타입의 값으로 evaluate 된다"
 - `t: T`

## Formal Definitions
### Typing Relation
 어떤 표현식에 대해서, 타입 규칙을 만족하는 가장 작은 relation. 이때
 `t: T`를 만족하는 T가 존재하면 term t는 *typable* 또는 *well typed*
 라고 한다.

### Typing Derivation
 어떤 표현식에 대해서 적용 가능한 타입 규칙을 적용한 인스턴스의 트리. 즉,

  - *Statement*: 프로그램의 타입을 정하는 formal assertions
  - *Typing Rules*: statement 사이의 implication
  - *Derivation*: 타입 규칙에 근거한 Deduction

## Safety = Progress + Preservation
