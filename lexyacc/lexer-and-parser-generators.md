# Lexer and Parser Generators (ocamllex, ocamlyacc)

 ocamllex은 (정규 표현식, 시맨틱 액션) 쌍의 집합으로부터 어휘 분석기를
 만들고, ocamlyacc은 (문법, 시맨틱 액션)으로부터 파서를 만든다.

## 1. Overview of ocamllex

 ocamllex 명령어는 lex 스타일로 정의된 정규 표현식과 거기 붙은 시맨틱
 액션들의 집합으로부터 어휘 분석기를 만든다. 입력 파일이 `lexer.mll`
 이라고 했을 때,

```bash
ocamllex lexer.mll
```

 을 실행하면 `lexer.ml` 파일에 어휘 생성기 OCaml 코드를 만든다. 이
 파일은 각각의 렉서 정의 진입점마다 하나의 렉싱 함수를 정의한다. 이
 함수들은 진입점이랑 같은 이름을 지닌다. 렉싱 함수는 렉서 버퍼를
 인자로 받아서 해당 진임접의 시맨틱 속성을 돌려준다.

 렉서 버퍼는 표준 라이브러리 `Lexing`에 구현된 추상 데이터
 타입이다. `Lexing.from_channel`, `Lexing.from_string`, 그리고
 `Lexing.from_function` 함수는 각각 입력 채널, 문자열, 그리고 입력
 함수로부터 읽어들여서 렉서 버퍼를 만든다.

 ocamlyacc 이 만드는 파서 생성기와 같이 쓰일 때, 시맨틱 액션은 생성된
 파싱 모듈에 의해 정의된 `token` 타입에 속하는 값을 계산한다.

### 1.1. Options

#### `-o output-file`
 ocamllex 이 만드는 파일 이름을 지정한다. 디폴트는 입력으로 들어오는
 파일에서 확장자를 `.ml` 로 바꾼 것이다.

#### `-q`
 조용한 모드로 실행한다. ocamllex은 보통 표준 출력에 도움이 될만한
 메시지를 뿌린다. 이걸 숨기는 옵션이다.


## 2. Syntax of lexer definitions

 렉서 정의의 형식은 다음과 같다.

```ocaml
{ header }

let ident = regexp ...
[refill { refill-handler }]
rule entrypoint [arg1 ... argn] =
  parse regexp { action }
    | ...
    | regexp { action }
and entrypoint [arg1 ... argn] =
  parse ...
and ...

{ trailer }
```

 주석은 OCaml과 동일하게 `(*`랑 `*)`로 사용한다. `parse` 키워드는
 `shortest` 키워드로 바꿀 수 있는데 이에 대한 시맨틱은 아래에서
 설명한다.

 리필 핸들러는 4.02 버전 이후에 추가된 최근 기능이다.

### 2.1. Header and trailer

 `header`와 `trailer` 부분은 중괄호로 감싼 임의의 OCaml 코드다. 둘 다
 생략 가능하다. 만약 있으면, `header` 부분은 최종 파일의 시작 부분에
 복사되고 `trailer`는 끝 부분에 복사된다. 보통 `header` 부분은
 액션에서 필요한 모듈을 `open` 지시자로 열거나 각각의 액션에서 사용될
 보조 함수를 정의한다.

### 2.2. Naming regular expressions

 `header`와 `entrypoint` 사이에는 자주 쓰이는 정규 표현식에 이름을
 붙일 수 있다. `let ident = regexp` 로 쓴다. 이 선언 이후의 정규
 표현식에서는 `regexp` 대신 짧게 `ident` 식별자를 쓸 수 있다.

### 2.3. Entry points

 진입점(`entrypoint`)의 이름은 반드시 유효한 OCaml 식별자, 즉 소문자로
 시작하는 문자열이어야 한다. 인자 `arg1`, ..., `argn` 도
 마찬가지다. 각각의 진입점은 n+1 개의 인자를 받는 하나의 OCaml 함수가
 되는데, 묵시적으로 `Lexing.lexbuf` 타입이 제일 마지막 인자로
 추가된다. 입력의 접두사가 규칙 중 하나와 일치할 때까지 이
 `Lexing.lexbuf` 인자로부터 문자들을 읽고, 규칙의 정규 표현식에
 매칭한다. 그러면 이에 해당하는 액션이 평가되고, 함수의 결과를
 돌려준다.

 만약 여러개의 정규 표현식이 입력의 접두사와 일치하면, 접두사와 "가장
 길게" 일치하는 정규 표현식이 선택된다. 이 길이마저 같은 경우,
 코드에서 먼저 나타나는 (즉 더 위쪽에 있는) 정규 표현식이 선택된다.

 하지만, 만약 렉서 규칙이 `parse` 대신 `shortest` 키워드로 정의된
 경우, "가장 짧게" 일치하는 정규 표현식을 선택한다. 이때 짧은 길이가
 같은 경우, 역시 먼저 나온 규칙이 선택된다. 이 기능은 보통의 어휘
 분석기에는 잘 사용되지 않을 거고, `ocamllex`를 단순한 텍스트 처리
 도구로 사용할 때 쓰일 수 있다.


### 2.4. Regular expressions

 정규 표현식은 lex 스타일이되 좀더 OCaml 문법에 맞게 정의된다.

 나머진 다 알겠는데 특이한 거 하나만 적자면 `regexp1 # regexp2`는 두
 문자 집합의 차와 매칭한다.

### 2.5. Actions

 액션은 임의의 OCaml 표현식이다. `as` 생성자를 이용해 정의된
 식별자들이 매칭된 문자열의 서브 파트에 묶여있는 문맥에서
 평가된다. 또한, `lexbuf` 역시 현재 렉서 버퍼에 묶인다. `Lexing`
 모듈과 함께 제공되는 렉서 버퍼에 대한 연산과 함께, `lexbuf`의
 전형적인 사용처는 다음과 같다.

 - `Lexing.lexeme lexbuf`: 매칭된 문자열을 돌려준다.
 - `Lexing,.lexeme_char lexbuf n`: 매칭된 문자열의 n 번째 문자를
   돌려준다. 0부터 시작한다.
 - `Lexing.lexeme_start lexbuf`: 매칭된 문자열의 시작 부분의 입력
   텍스트 안의 절대 위치를 돌려준다. 즉, 매칭된 문자열의 첫번째 문자의
   오프셋이다. 입력 텍스트의 첫번째 문자는 오프셋 0을 갖는다.
 - `Lexing.lexeme_end lexbuf`: 매칭된 문자열의 끝 부분의 입력 텍스트
   안의 절대 위치를 돌려준다. 즉, 매칭된 문자열 이후의 첫번째 문자의
   오프셋이다.
 - `entrypoint [exp1 ... expn] lexbuf`: 여기서 `entrypoint`는 같은
   렉서 정의 안의 다른 진입점 이름이다. 주어진 진입점에 대해서
   재귀적으로 렉서를 부른다. `lexbuf`가 마지막 인자임을
   주의하자. 중첩된 주석 같은걸 처리할 때 유용하다.

### 2.6. Variables in reqular expressions

 `as` 생성자는 다른 정규 표현식에서의 "그룹"과 비슷하다. 타입은
 `string`, `char`, `string option`, 또는 `char option`일 수 있다.

 일단 먼저 선형적이 패턴을 보자. 즉, `as` 로 묶인 모든 변수가 서로
 구별될 때이다. `regexp as ident` 에서, `ident`의 타입은 보통 `string`
 아니면 `string option` 이다. 단, `regexp` 가 문자 상수 하나,
 언더스코어, 길이 1짜리 문자열 상수, 문자 집합 사양, 등일 때는
 예외인데, 이때는 타입이 `char`아니면 `char option` 이다. 전체 규칙을
 매칭하는 일이 `as`에 묶인 서브 패턴에 매칭되지 않을 수도 있는 경우를
 위해 옵션 타입이 도입된다. 특히 `( regexp as ident )?` 와 같은 식이나
 `regexp1 | (regexp2 as ident) `와 같은 경우는 `as` 로 묶인 부분이
 매칭되지 않을 수도 있다.

 `as`에 묶인 변수에는 선형성 제한은 없다. 변수가 한번 이상 묶일 때는,
 이전 규칙이 다음과 같이 확장된다:

 - 모든 바인딩이 `char` 타입이 될 수 있으면 `char` 타입이다.
 - `as` 바인딩이 매칭되지 않고도 전체 표현식이 매칭될 수 있으면 변수는
   `option` 타입이다.

 예를 들어서,

  - `('a' as x) | ( 'a' (_ as x) )`: 변수 `x`는 전부 `char` 타입이므로
    `char` 타입이 된다.
  - `("ab" as x) | ( 'a' (_ as x) ? )`: 첫번째 `x`는 `string` 타입,
    두번째 `x`는 `char option` 타입이므로 더 큰 `string option` 타입이
    된다.

 어떤 경우에는, 매칭에 성공했어도 유일한 바인딩 집합을 산출하지 않을
 수도 있다. 예를 들어서, `aba`를 정규 표현식 `( ('a' | "ab") as x) (
 ("ba" | 'a') as y)` 에 매칭시키면, `x`가 `"ab"`, `y`가 `'a'`에
 매칭되는 바인딩이 나오거나 또는 `x`가 `'a'`, `y`가 `"ba"`에 매칭되는
 바인딩이 나올 수 있다. 이런 애매모호한 정규 표현식에 대해서
 ocamllex이 생성하는 오토마타는 그냥 가능한 바인딩 집합 중 하나를
 고른다. 선택된 바인딩 집합은 의도적으로 명시되지 않는다.

### 2.7. Refill handlers

 기본적으로 ocamllex이 렉싱 버퍼의 끝에 도달하면 조용히 `lexbuf`의
 `refill_buff` 함수를 호출하고 렉싱을 계속한다. 리필 액션을 조절할 수
 있는 일은 가끔 유용하다. 보통 비동기적 계산을 하는 라이브러리를 쓰면,
 블록킹 동기 연산을 피하기 위해서 지연 함수 안의 리필 액션을 덮어쓰고
 싶을 것이다.

 OCaml 4.02부터 `refill-handler`를 지정할 수 있는데 이는 리필이 발생할
 때 호출될 함수이다. 이 함수는 남아있는(continuation) 렉싱에 전달되어
 완전히 렉싱을 조절한다. 리필 액션으로 사용되는 OCaml 표현식은 다음
 타입을 가져야 한다:

```ocaml
(Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> 'a
```

 첫번째 인자는 ocamllex이 보통 수행하는 (버퍼를 리필하고 나서 다시
 렉싱 함수를 호출하는 등의) 프로세싱을 캡쳐하는 남아있는
 일(continuation)이다. 결과 타입은 `'a`의 인스턴스인데 렉싱 규칙의
 결과 타입을 합쳐야 한다.

 예를 들어서, 다음 렉서는 임의의 모나드를 파라미터로 받는다.

```ocaml
{
type token = EOL | INT of int | PLUS

module Make (M: sig
  type 'a t
  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val fail: string -> 'a t

  (* set up lexbuf *)
  val on_refill: Lexing.lexbuf -> unit t
end)
= struct
let refill_handler k lexbuf =
  M.bind (M.on_refill lexbuf) (fun () -> k lexbuf)
}

refill {refill_handler}

rule token = parse
| [' ' '\t']
  { token lexbuf }
| ['\n']
  { M.return EOL }
| ['0'-'9']+ as i
  { M.return (INT (int_of_string i)) }
| '+'
  { M.return PLUS }
| _
  { M.fail "unexpected character" }

{
end
}
```

### 2.8. Reserved identifiers

 `__ocaml_lex` 로 시작하는 식별자는 ocamllex 을 위한 예약어들이다.


## 3. Overview of ocamlyacc

 ocamlyacc 명령어는 yacc 스타일로 시맨틱 액션과 함께 명세된 문맥 자유
 문법으로부터 파서를 생성한다. 입력 파일이 `grammar.mly` 라고 할 때,

```bash
ocamlyacc options grammar.mly
```

 을 실행하면 `grammar.ml` 파일에 파서 OCaml 코드를 만들고, 인터페이스
 파일 `grammar.mli`를 함께 만든다.

 생성된 모듈은 문법의 한 진입점마다 하나의 파싱 함수를 정의한다. 이
 함수들은 진입점과 같은 이름을 갖는다. 파싱 함수는 어휘 분석기 (렉서
 버퍼로부터 토큰으로 가는 함수) 와 렉서 버퍼를 인자로 받아서, 진입점에
 해당하는 시맨틱 속성을 돌려준다. 어휘 분석기 함수는 보통 ocamllex에
 의해 렉서 명세로부터 생성된다. 토큰은 구체적인 `token` 타입이고 이는
 ocamlyacc에 의해 생성되는 `grammar.mli` 인터페이스에 정의되어 있다.

## 4. Syntax of grammar definitions

 문법 정의의 형식은 다음과 같다.

```ocaml
%{
  header
%}
  declarations
%%
  rules
%%
  trailer
```

 `declarations` 과 `rules` 영역에서의 주석은 C와 같은 `/*` 과 `*/`
 이고, `header`와 `trailer` 영역은 OCaml과 같은 `(*`과 `*)` 이다.

### 4.1. Header and trailer

 `header`와 `trailer` 사이의 코드는 `grammar.ml` 파일에 복사된다.


### 4.2. Declarations

 한 줄에 하나 씩 선언한다. `%` 사인으로 시작한다.

#### `%token constr ... constr`

  `constr ... constr` 심볼을 선언한다. 이것들은 다 터미널 심볼이다. 이
  심볼들은 구체적인 `token` 타입에서 상수 생성자로 추가된다.

#### `%token <typexpr> constr ... constr`

 `constr ... constr` 심볼을 주어진 타입으로 선언한다. 이 심볼들은
 `token` 타입을 만들 때 주어진 타입의 아규먼트를 받는 생성자로
 추가된다. `typexpr` 부분은 임의의 OCaml 타입 표현식인데, 단 모든 타입
 생성자의 이름이 표준 빌트인 타입을 제외하고는 `Modname.typename` 처럼
 모든 타입에 대해서 완전히 기술되어야 (fully qualified) 한다. `header`
 영역에 `open`을 통해서 모듈을 열었어도 반드시 기술해줘야 하는데,
 왜냐하면 `header` 영역은 결과 `.ml` 파일에만 복사될 뿐, `.mli`
 파일에는 복사되지 않기 때문이다.

#### `%start symbol ... symbol`

 `symbol ... symbol` 심볼들을 문법의 진입점으로 선언한다. 각각의
 진입점에서, 같은 이름을 가진 하나의 파싱 함수가 최종 모듈에
 정의된다. 진입점으로 선언되지 않은 논터미널 심볼들은 이런 파싱 함수를
 갖지 않는다. 시작 심볼들은 반드시 `%type` 명령어를 이용해 아래에
 타입을 명시해줘야 한다.

#### `%type <typexpr> symbol ... symbol`

 `symbol ... symbol` 심볼들에 대해서 시맨틱 속성의 타입을
 명시한다. 시작 심볼들은 반드시 이를 해줘야 한다. 다른 논터미널
 심볼들은 직접 손으로 적어줄 필요는 없다. 이런 타입들은 `-s` 옵션이
 주어져 있지 않았으면 OCaml 컴파일러에 의해서 자동으로
 추론된다. `typexpr` 부분은 임의의 OCaml 타입 표현식인데, 단 모든 타입
 생성자의 이름이 (`%token`에서 처럼) 완전히 기술되어야 한다.

#### `%left symbol ... symbol`
#### `%right symbol ... symbol`
#### `%nonassoc symbol ... symbol`

  심볼의 우선순위와 결합법칙을 지정한다. 같은 줄에 있는 심볼들은 모두
  같은 우선순위를 갖는다. 먼저 선언된 줄에 있는 심볼들이 더 높은
  우선순위를 갖는다. `%left`와 `%right` 는 각각 왼쪽 우선 결합과
  오른쪽 우선 결합을 지정한다. `%nonassoc`은 비결합성이다. 심볼은 보통
  토큰이다. 아니면 더미 논터미널일 수도 있는데, `%prec` 명령어를 규칙
  안에서 쓰기 위해서 종종 쓰인다.

  우선순위 선언은 reduce/reduce 또는 shift/reduce 충돌을 해결하기
  위해서 다음과 같은 방법으로 쓰인다.

 - 토큰과 규칙은 우선순위를 갖는다. 기본적으로 한 규칙의 우선순위는
   규칙의 가장 오른쪽에 있는 터미널의 우선순위이다. `%prec` 명령어를
   통해서 이를 덮어쓸 수 있다.
 - reduce/reduce 충돌은 소스 파일에 주어진 순서의 첫번째 규칙에 의해
   해결한다. ocamlyacc이 경고를 뿜는다.
 - shift/reduce 충돌은 shift 될 토큰의 우선순위를 reduce 될 규칙의
   우선순위를 비교해서 해결한다. 만약 규칙의 우선순위가 더 높으면, 그
   규칙이 reduce 된다; 만약 토큰의 우선순위가 더 높으면, 토큰이 shift
   된다.
 - 같은 우선순위를 가진 규칙과 토큰 사이의 shift/reduce 충돌은
   결합법칙을 통해 해결된다. 만약 토큰이 왼쪽 우선 결합인 경우, 파서는
   reduce 한다. 만약 토큰이 오른쪽 우선 결합인 경우, 파서는 shift
   한다. 만약 토큰이 비결합성이면, 파서는 문법 에러를 선언한다.
 - shift/reduce 충돌이 위의 방법들로 해결이 안되면, ocamlyacc은 경고를
   뿜고 파서는 항상 shift 한다.

### 4.3. Rules

 문법은 다음과 같다.

```ocaml
nonterminal :
    symbol ... symbol { semantic-action }
  | ...
  | symbol ... symbol { semantic-action }
;
```

 규칙은 `%prec` 심볼 명령어를 담을 수 있는데, 대상 심볼에 대한 규칙의
 기본 우선순위와 결합법칙을 덮어쓸 수 있다.

 시맨틱 액션은 정의된 논터미널에 붙은 시맨틱 속성을 만들기 위해
 평가되는 임의의 OCaml 표현식이다. 시맨틱 액션은 `$` 표기법을 통해
 규칙의 오른쪽에 있는 심볼의 시맨틱 속성에 접근할 수 있다. `$1`은
 왼쪽에서 첫번째 심볼의 속성이고, `$2`는 두번째고, 등등.

 규칙은 특수 심볼 `error`를 이용해서 재동기 시점(resynchronization
 points)을 지정할 수 있다.

 규칙 중간에 나타나는 액션은 지원하지 않는다.

 논터미널 심볼은 따옴표 (`'`) 로 끝날 수 없는 점만 빼면 일반적인 OCaml
 심볼이랑 같다.

### 4.4. Error handling

 다음과 같이 에러에서 회복하는 방법을 제공한다: 파서가 (적용할 수 있는
 문법이 없는) 에러 상태에 도달하면, `parse_error` 함수를 `"syntax
 error"` 라는 문자열을 인자로 호출한다. 기본 `parse_error` 함수는
 아무것도 안하고 그냥 리턴하는데, 따라서 에러 리커버리를 초기화
 한다. 개발자는 문법 파일의 `header` 영역에 맞춤형 `parse_error`
 함수를 정의할 수 있다.

 문법 액션 중 하나가 `Parsing.Parse_error` 예외를 발생할 때에도 파서는
 에러 회복 모드에 들어간다.

 에러 회복 모드에서, 파서는 에러 토큰을 shift 할 수 있을 때까지
 스택에서 상태를 버린다. 그러고 나서 받아들일 수 있는 세 개의 연속된
 토큰을 찾을 때까지 입력에서 토큰을 버리고, 이들의 첫번째를 처리하기
 시작한다. 만약 에러 토큰을 shift 할 수 있는 상태를 못찾으면, 파서는
 `Parsing.Parse_error` 예외를 발생시켜서 중단한다.

 에러 회복에 대해서는 yacc 을 더 살펴보도록.

## 5. Options

  ocamlyacc 명령어 옵션은 다음과 같다.

#### `-bprefix`
 출력 파일 이름을 `prefix.ml`, `prefix.mli`, `prefix.output` 으로
 설정한다.

#### `-`
 문법 명세를 표준입력에서 읽어들인다. 이때의 기본 출력 파일 이름은
 `stdin.ml`과 `stdin.mli` 이다.

#### `-- file`
 파일 이름이 `-`로 시작하더라도 `file`을 문법 명세 파일로 읽는다. 이
 옵션은 반드시 명령줄의 제일 마지막에 와야한다.
