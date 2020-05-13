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
