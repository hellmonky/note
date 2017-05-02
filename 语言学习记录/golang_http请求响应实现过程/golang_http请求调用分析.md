# golang学习纪要
golang的学习还是需要继续进行的，作为一个技术人员，技术的提升才是最为关键的。
比较好的方法就是使用实践+代码分析的方式，这样可以快速熟悉golang提供的基本语法和功能。

## 一个http连接处理流程：
自己按照当前的工作情况，需要再次熟悉golang相关的内容，并且结合web应用开发来完成需要补全的实践过程。所以目前就当前笔记本上，需要搭建GoLang的开发环境（golang安装文件和LiteIDE，或者vscode），然后按照教程来完成开发。

### 基本原理和开发教程：
HTTP请求的原理：
> - [Web工作方式](https://github.com/astaxie/build-web-application-with-golang/blob/master/zh/03.1.md)

![http包执行流程](http包执行流程.png)

通过这个教程，完整的描述了整个web服务器的运行原理和使用golang如何按照这个原理来完成功能的过程，对于自己加深互联网的认识有非常好的作用。

> - 创建Listen Socket, 监听指定的端口, 等待客户端请求到来。
> - Listen Socket接受客户端的请求, 得到Client Socket, 接下来通过Client Socket与客户端通信。
> - 处理客户端的请求, 首先从Client Socket读取HTTP请求的协议头, 如果是POST方法, 还可能要读取客户端提交的数据, 然后交给相应的handler处理请求, handler处理完毕准备好客户端需要的数据, 通过Client Socket写给客户端。

这整个的过程里面我们只要了解清楚下面三个问题，也就知道Go是如何让Web运行起来了

> - 如何监听端口？
> - 如何接受客户端的请求？
> - 如何分配handler？

将这三个问题按照步骤解决，就能完成自己搭建一个web服务器的过程（非常喜欢这个过程，因为按照原理进行分析，然后拆分实现的步骤，最后完成整个流程，非常漂亮）。

### 代码调用流程分析：
现在结合示例代码和golang提供的基础库：http(c:\go\src\net\http\server.go)，对完整的一个HTTP请求和响应流程进行分析。


#### 用户代码：
编辑httpserver.go文件，内容为：
```golang
package main

import (
	"io"
	"net/http"
)

type helloHandler struct{}

func (h *helloHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	w.Write([]byte("Hello, world!"))
}

func main() {
	http.Handle("/", &helloHandler{})
	http.ListenAndServe(":9090", nil)
}
```
用户端代码分析：
golang 的标准库 net/http 提供了 http 编程有关的接口，封装了内部TCP连接和报文解析的复杂琐碎的细节，使用者只需要和 http.request 和 http.ResponseWriter 两个对象交互就行。也就是说，我们只要写一个 handler，请求会通过参数传递进来，而它要做的就是根据请求的数据做处理，把结果写到 Response 中。
运行 go run httpserver.go，我们的服务器就会监听在本地的 9090 端口，对所有的请求都会返回 hello, world!

关键就在于：我们只要实现的一个 Handler，它的接口原型是（也就是说只要实现了 ServeHTTP 方法的对象都可以作为 Handler）：
```golang
type Handler interface {
	ServeHTTP(ResponseWriter, *Request)
}
```
然后，注册到对应的路由路径上就 OK 了。

整个流程涉及到两个函数：
> - http.HandleFunc接受两个参数：第一个参数是字符串表示的 url 路径，第二个参数是该 url 实际的处理对象。
> - http.ListenAndServe 监听在某个端口，启动服务，准备接受客户端的请求（第二个参数这里设置为 nil，这里也不要纠结什么意思，后面会有讲解）。每次客户端有请求的时候，把请求封装成 http.Request，调用对应的 handler 的 ServeHTTP 方法，然后把操作后的 http.ResponseWriter 解析，返回到客户端。

上面的代码没有什么问题，但是有一个不便：每次写 Handler 的时候，都要定义一个类型，然后编写对应的 ServeHTTP 方法，这个步骤对于所有 Handler 都是一样的。重复的工作总是可以抽象出来，net/http 也正这么做了，它提供了 http.HandleFunc 方法，允许直接把特定类型的函数作为 handler。上面的代码可以改成：
```golang
package main

import (
	"io"
	"net/http"
)

func helloHandler(w http.ResponseWriter, req *http.Request) {
	io.WriteString(w, "hello, world!\n")
}

func main() {
	http.HandleFunc("/", helloHandler)
	http.ListenAndServe(":9090", nil)
}
```
其实，HandleFunc 只是一个适配器：
```golang
// The HandlerFunc type is an adapter to allow the use of
// ordinary functions as HTTP handlers. If f is a function
// with the appropriate signature, HandlerFunc(f) is a
// Handler that calls f.
type HandlerFunc func(ResponseWriter, *Request)

// ServeHTTP calls f(w, r).
func (f HandlerFunc) ServeHTTP(w ResponseWriter, r *Request) {
	f(w, r)
}
```
自动给 f 函数添加了 HandlerFunc 这个壳，最终调用的还是 ServerHTTP，只不过会直接使用 f(w, r)。这样封装的好处是：使用者可以专注于业务逻辑的编写，省去了很多重复的代码处理逻辑。如果只是简单的 Handler，会直接使用函数；如果是需要传递更多信息或者有复杂的操作，会使用上部分的方法。
为了更为清楚的看出到底如何调用的，还可以这样调用：
```golang
func main() {
    // 通过 HandlerFunc 把函数转换成 Handler 接口的实现对象
    hh := http.HandlerFunc(helloHandler)
	http.Handle("/", hh)
	http.ListenAndServe(":9090", nil)
}
```

参考文档：
> - [go http 服务器编程](http://cizixs.com/2016/08/17/golang-http-server-side)

#### 调用分析：
用户端代码：
```golang
package main

import (
	"fmt"
	"log"
	"net/http"
	"strings"
)

func sayhelloName(w http.ResponseWriter, r *http.Request) {
	r.ParseForm()       //解析参数，默认是不会解析的
	fmt.Println(r.Form) //这些信息是输出到服务器端的打印信息
	fmt.Println("path", r.URL.Path)
	fmt.Println("scheme", r.URL.Scheme)
	fmt.Println(r.Form["url_long"])
	for k, v := range r.Form {
		fmt.Println("key:", k)
		fmt.Println("val:", strings.Join(v, ""))
	}
	fmt.Fprintf(w, "Hello world!") //这个写入到w的是输出到客户端的
}

func main() {
	http.HandleFunc("/", sayhelloName)       //设置访问的路由
	err := http.ListenAndServe(":9090", nil) //设置监听的端口
	if err != nil {
		log.Fatal("ListenAndServe: ", err)
	}
}
```
1 用户创建一个ServerHTTP的实例，完成这个实例功能的是函数sayhelloName，也就是说通过这个函数将会对HTTP请求的响应进行处理。
2 然后用户通过ListenAndServe开启HTTP服务。

##### 监听端口：
ListenAndServer这个函数底层是通过初始化一个server对象，然后调用了net.Listen("tcp", addr)，也就是底层用TCP协议搭建了一个服务，然后监控我们设置的端口。
查看ListenAndServe这个函数的底层实现：
```golang
func ListenAndServe(addr string, handler Handler) error {
	server := &Server{Addr: addr, Handler: handler}
	return server.ListenAndServe()
}
```
可以看出，底层调用了server.ListenAndServe()来完成端口的监控，再进入：server.ListenAndServe()
```golang
func (srv *Server) ListenAndServe() error {
	addr := srv.Addr
	if addr == "" {
		addr = ":http"
	}
	ln, err := net.Listen("tcp", addr)
	if err != nil {
		return err
	}
	return srv.Serve(tcpKeepAliveListener{ln.(*net.TCPListener)})
}
```
其中使用了：net.Listen("tcp", addr) 来对addr所指定的端口进行了监听，也就是说通过TCP协议直接进行了端口的监听（没有通过ngix等代理完成）。
上述就完成了第一步：监听端口的要求。

##### 接受用户请求：
监控之后如何接收客户端的请求呢？上面代码执行监控端口之后，调用了srv.Serve(net.Listener)函数，这个函数就是处理接收客户端的请求信息。
进入：srv.Serve(tcpKeepAliveListener{ln.(*net.TCPListener)})看一下：
```golang
func (srv *Server) Serve(l net.Listener) error {
	defer l.Close()
	if fn := testHookServerServe; fn != nil {
		fn(srv, l)
	}
	var tempDelay time.Duration // how long to sleep on accept failure

	if err := srv.setupHTTP2_Serve(); err != nil {
		return err
	}

	srv.trackListener(l, true)
	defer srv.trackListener(l, false)

	baseCtx := context.Background() // base is always background, per Issue 16220
	ctx := context.WithValue(baseCtx, ServerContextKey, srv)
	ctx = context.WithValue(ctx, LocalAddrContextKey, l.Addr())
	for {
		rw, e := l.Accept()
		if e != nil {
			select {
			case <-srv.getDoneChan():
				return ErrServerClosed
			default:
			}
			if ne, ok := e.(net.Error); ok && ne.Temporary() {
				if tempDelay == 0 {
					tempDelay = 5 * time.Millisecond
				} else {
					tempDelay *= 2
				}
				if max := 1 * time.Second; tempDelay > max {
					tempDelay = max
				}
				srv.logf("http: Accept error: %v; retrying in %v", e, tempDelay)
				time.Sleep(tempDelay)
				continue
			}
			return e
		}
		tempDelay = 0
		c := srv.newConn(rw)
		c.setState(c.rwc, StateNew) // before Serve can return
		go c.serve(ctx)
	}
}
```
这个函数里面起了一个for{}，对于每一个用户发起的HTTP请求：
> - 首先通过Listener接收请求：rw, e := l.Accept()，然后进行参数的检查和设置；
> - 其次创建一个Conn：srv.newConn(rw)；
> - 最后单独开了一个goroutine，把这个请求的数据当做参数扔给这个conn去服务：go c.serve()。

这个就是高并发体现了，用户的每一次请求都是在一个新的goroutine去服务，相互不影响。
golang中的关键字go是语言提供的多线程关键字，表示将当前的执行代码开启一个新的线程，原生的支持多线程并发的开发。

##### 处理用户请求：
如何使用指定的方法来处理用户请求？我们需要进入启动的server中看conn如何对于用户请求的处理过程。
```golang
func (c *conn) serve(ctx context.Context) {
	c.remoteAddr = c.rwc.RemoteAddr().String()
	defer func() {
		if err := recover(); err != nil && err != ErrAbortHandler {
			const size = 64 << 10
			buf := make([]byte, size)
			buf = buf[:runtime.Stack(buf, false)]
			c.server.logf("http: panic serving %v: %v\n%s", c.remoteAddr, err, buf)
		}
		if !c.hijacked() {
			c.close()
			c.setState(c.rwc, StateClosed)
		}
	}()

	if tlsConn, ok := c.rwc.(*tls.Conn); ok {
		if d := c.server.ReadTimeout; d != 0 {
			c.rwc.SetReadDeadline(time.Now().Add(d))
		}
		if d := c.server.WriteTimeout; d != 0 {
			c.rwc.SetWriteDeadline(time.Now().Add(d))
		}
		if err := tlsConn.Handshake(); err != nil {
			c.server.logf("http: TLS handshake error from %s: %v", c.rwc.RemoteAddr(), err)
			return
		}
		c.tlsState = new(tls.ConnectionState)
		*c.tlsState = tlsConn.ConnectionState()
		if proto := c.tlsState.NegotiatedProtocol; validNPN(proto) {
			if fn := c.server.TLSNextProto[proto]; fn != nil {
				h := initNPNRequest{tlsConn, serverHandler{c.server}}
				fn(c.server, tlsConn, h)
			}
			return
		}
	}

	// HTTP/1.x from here on.

	ctx, cancelCtx := context.WithCancel(ctx)
	c.cancelCtx = cancelCtx
	defer cancelCtx()

	c.r = &connReader{conn: c}
	c.bufr = newBufioReader(c.r)
	c.bufw = newBufioWriterSize(checkConnErrorWriter{c}, 4<<10)

	for {
		w, err := c.readRequest(ctx)
		if c.r.remain != c.server.initialReadLimitSize() {
			// If we read any bytes off the wire, we're active.
			c.setState(c.rwc, StateActive)
		}
		if err != nil {
			const errorHeaders = "\r\nContent-Type: text/plain; charset=utf-8\r\nConnection: close\r\n\r\n"

			if err == errTooLarge {
				// Their HTTP client may or may not be
				// able to read this if we're
				// responding to them and hanging up
				// while they're still writing their
				// request. Undefined behavior.
				const publicErr = "431 Request Header Fields Too Large"
				fmt.Fprintf(c.rwc, "HTTP/1.1 "+publicErr+errorHeaders+publicErr)
				c.closeWriteAndWait()
				return
			}
			if isCommonNetReadError(err) {
				return // don't reply
			}

			publicErr := "400 Bad Request"
			if v, ok := err.(badRequestError); ok {
				publicErr = publicErr + ": " + string(v)
			}

			fmt.Fprintf(c.rwc, "HTTP/1.1 "+publicErr+errorHeaders+publicErr)
			return
		}

		// Expect 100 Continue support
		req := w.req
		if req.expectsContinue() {
			if req.ProtoAtLeast(1, 1) && req.ContentLength != 0 {
				// Wrap the Body reader with one that replies on the connection
				req.Body = &expectContinueReader{readCloser: req.Body, resp: w}
			}
		} else if req.Header.get("Expect") != "" {
			w.sendExpectationFailed()
			return
		}

		c.curReq.Store(w)

		if requestBodyRemains(req.Body) {
			registerOnHitEOF(req.Body, w.conn.r.startBackgroundRead)
		} else {
			if w.conn.bufr.Buffered() > 0 {
				w.conn.r.closeNotifyFromPipelinedRequest()
			}
			w.conn.r.startBackgroundRead()
		}

		// HTTP cannot have multiple simultaneous active requests.[*]
		// Until the server replies to this request, it can't read another,
		// so we might as well run the handler in this goroutine.
		// [*] Not strictly true: HTTP pipelining. We could let them all process
		// in parallel even if their responses need to be serialized.
		// But we're not going to implement HTTP pipelining because it
		// was never deployed in the wild and the answer is HTTP/2.
		serverHandler{c.server}.ServeHTTP(w, w.req)
		w.cancelCtx()
		if c.hijacked() {
			return
		}
		w.finishRequest()
		if !w.shouldReuseConnection() {
			if w.requestBodyLimitHit || w.closedRequestBodyEarly() {
				c.closeWriteAndWait()
			}
			return
		}
		c.setState(c.rwc, StateIdle)
		c.curReq.Store((*response)(nil))

		if !w.conn.server.doKeepAlives() {
			// We're in shutdown mode. We might've replied
			// to the user without "Connection: close" and
			// they might think they can send another
			// request, but such is life with HTTP/1.1.
			return
		}

		if d := c.server.idleTimeout(); d != 0 {
			c.rwc.SetReadDeadline(time.Now().Add(d))
			if _, err := c.bufr.Peek(4); err != nil {
				return
			}
		}
		c.rwc.SetReadDeadline(time.Time{})
	}
}
```
conn首先会解析request：w, err := c.readRequest(ctx)
然后获取相应的handler：serverHandler{c.server}.ServeHTTP(w, w.req)，也就是我们刚才在调用函数ListenAndServe时候的第二个参数，我们前面例子传递的是nil，也就是为空。
进入ServeHTTP(w, w.req)：
```golang
func (sh serverHandler) ServeHTTP(rw ResponseWriter, req *Request) {
	handler := sh.srv.Handler
	if handler == nil {
		handler = DefaultServeMux
	}
	if req.RequestURI == "*" && req.Method == "OPTIONS" {
		handler = globalOptionsHandler{}
	}
	handler.ServeHTTP(rw, req)
}
```
因为传入参数为空，那么默认获取handler = DefaultServeMux，这个变量就是一个路由器，它用来匹配url跳转到其相应的handle函数。我们调用的代码里面第一句不是调用了http.HandleFunc("/", sayhelloName)嘛。这个作用就是注册了请求/的路由规则，当请求uri为"/"，路由就会转到函数sayhelloName，DefaultServeMux会调用ServeHTTP方法，这个方法内部其实就是调用sayhelloName本身，最后通过写入response的信息反馈到客户端。

完整的调用流程图为：

![一个http连接golang的处理流程](一个http连接golang的处理流程.png)




