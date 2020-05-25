# Unix System Programming in OCaml

## 1. Generalities
### 1.1. Modules `Sys` and `Unix`
 `Sys`에는 유닉스나 다른 운영체제에 공통 인터페이스가 담겨 있고
 `Unix`는 유닉스 특화 인터페이스가 담겨 있다.

 각 모듈은 기존의 `Pervasives` 모듈의 식별자를 재정의 하기도
 한다. 예를 들면 `Pervasives.stdin` 이랑 `Unix.stdin`은 다른 타입이다.

### 1.2. Interface with the calling program
 쉘에서 프로그램을 실행하면, 쉘은 *아규먼트*와 *환경*을 프로그램에게
 전달해준다. 아규먼트는 커맨드 라인에서 방금 실행한 프로그램의 커맨드
 이후의 모든 단어들이다. 환경은 `variable=value` 형식을 가진 문자열의
 집합으로, 환경 변수의 글로벌 바인딩을 나타낸다. `csh`에서는 `setenv
 var=val` 으로, `sh` 에서는 `var=val; export var`로 가능하다.

#### `var argv: string array`
 아규먼트는 `Sys.argv`에 문자열 배열로 전달된다.

#### `val environment : unit -> string array`
 프로그램의 환경은 `Unix.environment` 함수로 얻을 수 있다.

#### `val getenv : string -> string`
 환경 변수를 얻는 좀더 편한 방법으로는 `Sys.getenv`가
 있다. `Sys.getenv v`는 변수 `v` 에 연결된 환경 변수 값을 돌려주는데,
 바인딩이 없으면 `Not_found` 예외를 던진다.

 예시는 프로그램의 아규먼트를 출력하는 [에코
 프로그램](sample/ex_1.2_echo.ml)이다.

#### `val exit : int -> 'a`
 인자 `int`는 프로그램을 호출한 프로그램에게 돌려주는 리턴 코드
 값이다. 관습적으로 `0`은 모든 게 잘 됐다는 걸 뜻하고, 그 외의 `0`이
 아닌 코드는 오류라는 신호이다. 조건문에서 `sh` 쉘은 리턴 코드 `0`을
 불리언 값 "True"로 해석하고, 모든 `0`이 아닌 값을 불리언 값 "False"로
 해석한다. 어떤 프로그램이 담고 있는 모든 표현식을 정상적으로 실행하고
 종료한다면, 암묵적으로 `exit 0`을 호출한다. 어떤 프로그램이 중간에
 어떤 예외가 발생했지만 그것을 놓친 채로 종료되면, 암묵적으로 `exit
 2`를 호출한다. `exit` 함수는 항상 (쓰기 위해 열려 있는) 모든 채널의
 버퍼를 비운다 (flush). `at_exit` 함수는 프로그램이 종료할 때 수행할
 다른 동작을 예약 등록할 수 있다.

#### `val at_exit : (unit -> unit) -> unit`
 마지막에 등록한 함수부터 호출된다. 즉, 스택이다. `at_exit`로 등록한
 함수는 등록해제할 수 없다.

### 1.3. Error handling
 특별한 일이 없으면, `Unix` 모듈의 모든 함수는 오류 시에
 `Unix_error`를 던진다.

#### `exception Unix_error of error * string * string`
 두번째 아규먼트는 이 오류를 던진 시스템 콜의 이름이다. 세번째
 아규먼트는 (가능한 경우에만) 오류가 발생한 오브젝트이다. 예를 들어서,
 파일 이름을 아규먼트로 받는 시스템 콜의 경우, 이 파일 이름이 세번째
 아규먼트에 담긴다. 마지막으로 첫번째 아규먼트는 에러 코드로 오류의
 성질을 나타낸다.

#### `type error = E2BIG | EACCES | EAGAIN | ... | EUNKNOWNERR of int`
 이 타입 생성자는 POSIX 표준과 UNIX98, BSD의 오류에 쓰인 이름과 의미를
 따른다. 이외의 모든 오류는 `EUNKOWNERR` 생성자를 쓴다.

 예외의 시맨틱에 따라, 명확하게 예측하지 못해서 `try` 를 이용해
 다뤄지지 않은 오류는 프로그램의 최상단까지 전파되어서 프로그램을
 이르게 종료시켜버린다. 작은 어플리케이션에서는 예측 못한 오류를
 치명적인 오류로 다루는건 좋은 방법이다. 하지만, 에러를 분명하게
 드러내는게 좋다. 이를 위해서 `Unix` 모듈은 아래 함수를 제공한다.

#### `val handle_unix_error : ('a -> 'b) -> 'a -> 'b`
 `handle_unix_error f x`는 함수 `f`를 아규먼트 `x`로 호출한다. 만약
 이게 `Unix_error` 예외를 일으키면, 그 오류를 설명하는 메시지를
 출력하고 프로그램은 `exit 2`로 종료된다. 전형적인 사용법은
 `handle_unix_error prog () ;;` 처럼 `prog : unit -> unit` 함수가
 프로그램의 본체를 실행하는 경우이다. `handle_unix_error` 함수는
 [이렇게](sample/ex_1.3_unix_error.ml) 구현할 수 있다.
