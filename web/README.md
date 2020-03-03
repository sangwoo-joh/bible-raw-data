# Web

## Glossary
### 웹 서버
 클라이언트가 요청한 리소스(e.g. html, 이미지 등)를 전달하는
 서버. 기본적으로 정적이다.

### CGI (Common Gateway Interface)
 웹 서버에서 어플리케이션을 실행하기 위한 인터페이스. 정적인 웹 서버를
 **동적**으로 동작하도록 하기 위해서 등장하였다.

 서버 프로그램과 외부 프로그램 사이의 인터페이스.

 기존에는, 웹서버가 있고 클라이언트에서 외부 프로그램을 필요로 하는
 요청이 들어오면, CGI를 통해 외부 프로그램을 실행하여 요청에 응답했다.

 요즘에는, 웹서버에 CGI 인터프리터를 내장해서, 따로 프로세스를 fork
 하여, 외부 프로그램을 실행시키지 않고 내부에서 다 처리한다.

 * 요청 -> 웹 서버 (Apache, nginx, ...) -> 어플리케이션

### WAS (Web Application Server)
 웹 서버가 동적으로 동작하면 WAS 이다. 즉, 웹 서버 + CGI.

 * 요청 -> 웹 서버 -> 웹 어플리케이션 서버 (Tomcat, ...) ->
   어플리케이션

 어플리케이션 서버가 어플리케이션의 실행 결과를 웹 서버에 전달해주면,
 웹 서버가 이 결과를 클라이언트에 전송한다.


### WSGI (Web Server Gateway Interface)
 **파이썬**에서 어플리케이션, 즉 파이썬 프로그램 (웹 어플리케이션) 과
 웹 서버가 통신하기 위한 인터페이스 혹은 프로토콜.

 * 요청 -> 웹 서버 -> WSGI Server (middleware) -> WSGI를 지원하는 웹
   어플리케이션 (Django, flask, ...)
 * middleware: mod_wsgi, uwsgi, gunicorn, twisted.web, tornado, ...
