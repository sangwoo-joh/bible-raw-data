# Inter-process communications

## 1. Pipes

 일반 파일은 병렬로 실행되는 프로세스에게 만족스러운 커뮤니케이션
 수단이 아니다. 예를 들면 여러 명의 읽는 애랑 한 명의 쓰는 애가 있는
 상황을 생각해봐라. 어떤 파일 하나가 커뮤니케이션 수단으로 사용된다고
 하면, 읽는 애는 어느 순간 파일이 더 이상 자라지 않는 걸 깨닫게 되는데
 (즉, `read`가 0을 리턴한다), 이때 얘는 쓰는 애가 쓰는걸 다 끝냈는지
 아니면 단순히 쓰기 위한 데이터를 계산하느라 바쁜지 알 수가
 없다. 게다가, 파일에는 이때까지 전송한 모든 데이터가 다 들어있어서,
 쓸데없는 디스크 공간도 차지한다.

 *파이프*는 이런 종류의 커뮤니케이션에 알맞는 메커니즘을
 제공한다. 파이프 하나는 두 개의 파일 디스크립터로 구성된다. 하나는
 파이프의 출력이고, 다른 하나는 파이프의 입력이다. 파이프는 시스템 콜
 `pipe`로 만든다.

```ocaml
val pipe : unit -> file_descr * file_descr
```

 이걸 호출하면 `(fd_in, fd_out)` 쌍을 얻을 수 있는데, `fd_in`은
 파이프의 출력에 대해서 *읽기 모드*로 열린 파일 디스크립터이고,
 `fd_out`은 파이프의 입력에 대해서 *쓰기 모드*로 열린 파일
 디스크립터이다. 파이프 그 자체는 커널의 내부 오브젝트로 이렇게 생성한
 두 개의 파일 디스크립터로만 접근할 수 있다. 그리고 파일 시스템 상에서
 이름을 갖지 않는다.

 `fd_out` 디스크립터에 데이터를 써서 파이프를 채우고, 파이프에 채워진
 데이터는 `fd_in` 디스크립터를 통해 읽어내는 구조다.

 파이프는 큐다. 제일 처음 쓰여진 데이터가 제일 처음 읽힌다. `write`
 함수를 파이프의 입력 디스크립터 (`fd_out`)에 호출하여 쓰는 일은
 파이프에 데이터를 채우거나 파이프가 꽉찬 경우 블럭된다. 이때 다른
 프로세스가 이 파이프에서 (`fd_in` 으로부터) 데이터를 충분히 읽어서
 `write` 하려던 데이터 크기만큼 파이프가 비워질 때까지
 블럭된다. `read`를 파이프의 출력 디스크립터 (`fd_in`)에 호출하여 읽는
 일은 파이프에서 데이터를 빼낸다. 파이프가 비어있으면, `read` 호출은
 최소 한 바이트가 채워질때까지 블럭된다. 그런 다음 `read` 호출로
 읽어들이려던 바이트 수만큼이 채워질 때까지 기다리지 않고 즉시
 리턴한다.

 같은 프로세스에서 쓰고 읽는 경우 파이프는 쓸모없다. 그런 프로세스는
 엄청난 크기의 쓰기 작업이나 빈 파이프에서 읽는 작업으로 영원히 블럭될
 가능성이 높다. 그래서 보통은 서로 다른 프로세스끼리 읽고 쓰는 일에
 쓰인다. 파이프는 이름이 없기 때문에, 이런 프로세스는 반드시 파이프를
 만든 프로세스에서 fork 해야 된다. 실제로, 파이프의 두 개의 파일
 디스크립터는, 다른 파일 디스크립터와 마찬가지로, `fork` 호출 시에
 복제되고 이로 인해 자식과 부모 프로세스가 같은 파이프를 참조할 수
 있는 것이다.

```ocaml
let (fd_in, fd_out) = Unix.pipe () in
match fork () with
| 0 -> close fd_in; ... write fd_out buffer1 offset1 count1 ...
| pid -> close fd_out; ... read fd_in buffer2 offset2 count2 ...
```

 `fork` 하고나면 파이프의 입출력에 대해서 파일 디스크립터가 각각 두
 개씩 열리게 된다. 위의 코드에서는 자식이 쓰는 애가 되고 부모가 읽는
 애가 된다. 따라서 자식은 `fd_in` 디스크립터를 닫아서 파이프의
 출력으로부터 읽지 않도록 하고, 부모는 `fd_out` 디스크립터를 닫아서
 파이프의 입력에다 쓰지 않도록 한다. 이를 통해 부모와 자식의 메모리를
 서로소로 만든다 (Disjoint). 이는 파일 디스크립터도 정리하고,
 프로그래밍 실수도 방지하는 방법이다. 결과적으로는 다음과 같이 된다.


 `fd_out` (child) (input) -> PIPE -> `fd_in` (parent) (output)


 자식이 `fd_out`에 데이터를 쓰면 부모가 `fd_in`에서 데이터를 읽을 수
 있다.


 파이프의 입력에 대한 모든 디스크립터가 닫혀 있고 파이프가 비어있으면,
 파이프의 출력에 `read`를 호출하면 0을 리턴하여 파일의 끝을 알린다
 (EOF). 파이프의 출력에 대한 모든 디스크립터가 닫혀 있으면, 파이프의
 입력에 `write`를 호출하면 쓰는 작업을 죽여버린다. 더 정확히는, 커널이
 `write`를 호출 중인 프로세스에 `sigpipe` 시그널을 보내서 이 시그널의
 디폴트 핸들러가 프로세스를 죽여버린다. 만약 `sigpipe` 시그널 핸들러가
 바뀌면, `write` 호출은 `EPIPE` 에러로 종료된다.