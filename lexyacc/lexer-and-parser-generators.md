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
