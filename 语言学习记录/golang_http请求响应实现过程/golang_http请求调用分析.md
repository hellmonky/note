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

上面的代码没有什么问题，但是有一个不便：每次写 Handler 的时候，都要定义一个类型，然后编写对应的 ServeHTTP 方法，这个步骤对于所有 Handler 都是一样的。重复的工作总是可以抽象出来，net/http 也正这么做了，它提供了 http.HandleFunc 方法，允许直接把特定类型的函数作为 handler。
在http包里面还定义了一个类型HandlerFunc，这个类型默认就实现了ServeHTTP这个接口，即我们调用了HandlerFunc(f),强制类型转换f成为HandlerFunc类型，这样f就拥有了ServeHTTP方法。
上面的代码可以改成：
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


## golang的http核心：Conn和ServeMux
也就是接受用户HTTP请求的链接和处理用户请求的路由器。

### Conn：
Go为了实现高并发和高性能, 使用了goroutines来处理Conn的读写事件, 这样每个请求都能保持独立，相互不会阻塞，可以高效的响应网络事件。这是Go高效的保证。
在等待用户请求的代码中，使用for循环获取用户请求，然后对每一个请求新建Conn。这个Conn里面保存了该次请求的信息，然后再传递到对应的handler，该handler中便可以读取到相应的header信息，这样保证了每个请求的独立性。

### ServeMux：
Conn通过调用server方法，将用户的HTTP请求通过路由器把本次请求的信息传递到了后端的处理函数。如果没有设置路由器，就会使用golang内置的默认路由器进行处理。
那么该如何自定义路由器？首先看看ServerMux这个结构体：
```golang
type ServeMux struct {
	mu    sync.RWMutex          //锁，由于请求涉及到并发处理，因此这里需要一个锁机制
	m     map[string]muxEntry   // 路由规则，一个string对应一个mux实体，这里的string就是注册的路由表达式
	hosts bool                  // 是否在任意的规则中带有host信息
}
```
> 注意：golang中 map 对象的声明形式为 map[TypeOfKey]TypeOfValue，所以上述结构体类型中的m为map类型，key的类型为string，value的类型为muxEntry。

然后muxEntry结构定义为：
```golang
type muxEntry struct {
	explicit bool       // 是否精确匹配
	h        Handler    // 这个路由表达式对应哪个handler
	pattern  string     // 匹配字符串
}
```
然后查看Handler的定义为：
```golang
type Handler interface {
	ServeHTTP(ResponseWriter, *Request) // 路由实现器
}
```
Handler是一个接口，路由器的功能需要实现这个接口。
在http包里面还定义了一个类型HandlerFunc，也就是上述分析中看到的强制类型转换函数：
```golang
// ServeHTTP calls f(w, r).
func (f HandlerFunc) ServeHTTP(w ResponseWriter, r *Request) {
	f(w, r)
}
```
这个类型默认就实现了ServeHTTP这个接口，即我们调用了HandlerFunc(f)，强制类型转换f成为HandlerFunc类型，这样f就拥有了ServeHTTP方法。

路由器里面存储好了相应的路由规则之后，那么具体的请求又是怎么分发的呢？请看下面的代码，默认的路由器实现了ServeHTTP：
```golang
// ServeHTTP dispatches the request to the handler whose
// pattern most closely matches the request URL.
func (mux *ServeMux) ServeHTTP(w ResponseWriter, r *Request) {
	if r.RequestURI == "*" {
		if r.ProtoAtLeast(1, 1) {
			w.Header().Set("Connection", "close")
		}
		w.WriteHeader(StatusBadRequest)
		return
	}
	h, _ := mux.Handler(r)
	h.ServeHTTP(w, r)
}
```
如上所示路由器接收到请求之后，如果是*那么关闭链接，不然调用mux.Handler(r)返回对应设置路由的处理Handler，然后执行h.ServeHTTP(w, r)。
也就是调用对应路由的handler的ServerHTTP接口，那么mux.Handler(r)怎么处理的呢？
```golang
// Handler returns the handler to use for the given request,
// consulting r.Method, r.Host, and r.URL.Path. It always returns
// a non-nil handler. If the path is not in its canonical form, the
// handler will be an internally-generated handler that redirects
// to the canonical path.
//
// Handler also returns the registered pattern that matches the
// request or, in the case of internally-generated redirects,
// the pattern that will match after following the redirect.
//
// If there is no registered handler that applies to the request,
// Handler returns a ``page not found'' handler and an empty pattern.
func (mux *ServeMux) Handler(r *Request) (h Handler, pattern string) {
	if r.Method != "CONNECT" {
		if p := cleanPath(r.URL.Path); p != r.URL.Path {
			_, pattern = mux.handler(r.Host, p)
			url := *r.URL
			url.Path = p
			return RedirectHandler(url.String(), StatusMovedPermanently), pattern
		}
	}

	return mux.handler(r.Host, r.URL.Path)
}

// handler is the main implementation of Handler.
// The path is known to be in canonical form, except for CONNECT methods.
func (mux *ServeMux) handler(host, path string) (h Handler, pattern string) {
	mux.mu.RLock()
	defer mux.mu.RUnlock()

	// Host-specific pattern takes precedence over generic ones
	if mux.hosts {
		h, pattern = mux.match(host + path)
	}
	if h == nil {
		h, pattern = mux.match(path)
	}
	if h == nil {
		h, pattern = NotFoundHandler(), ""
	}
	return
}
```
原来他是根据用户请求的URL和路由器里面存储的map去匹配的，当匹配到之后返回存储的handler，调用这个handler的ServeHTTP接口就可以执行到相应的函数了。

然后继续看看Handler 函数接受的两个参数：http.Request 和 http.ResponseWriter。
Request 就是封装好的客户端请求，包括 URL，method，header 等等所有信息，以及一些方便使用的方法：
```golang
// A Request represents an HTTP request received by a server
// or to be sent by a client.
//
// The field semantics differ slightly between client and server
// usage. In addition to the notes on the fields below, see the
// documentation for Request.Write and RoundTripper.
type Request struct {
	// Method specifies the HTTP method (GET, POST, PUT, etc.).
	// For client requests an empty string means GET.
	Method string

	// URL specifies either the URI being requested (for server
	// requests) or the URL to access (for client requests).
	//
	// For server requests the URL is parsed from the URI
	// supplied on the Request-Line as stored in RequestURI.  For
	// most requests, fields other than Path and RawQuery will be
	// empty. (See RFC 2616, Section 5.1.2)
	//
	// For client requests, the URL's Host specifies the server to
	// connect to, while the Request's Host field optionally
	// specifies the Host header value to send in the HTTP
	// request.
	URL *url.URL

	// The protocol version for incoming server requests.
	//
	// For client requests these fields are ignored. The HTTP
	// client code always uses either HTTP/1.1 or HTTP/2.
	// See the docs on Transport for details.
	Proto      string // "HTTP/1.0"
	ProtoMajor int    // 1
	ProtoMinor int    // 0

	// Header contains the request header fields either received
	// by the server or to be sent by the client.
	//
	// If a server received a request with header lines,
	//
	//	Host: example.com
	//	accept-encoding: gzip, deflate
	//	Accept-Language: en-us
	//	fOO: Bar
	//	foo: two
	//
	// then
	//
	//	Header = map[string][]string{
	//		"Accept-Encoding": {"gzip, deflate"},
	//		"Accept-Language": {"en-us"},
	//		"Foo": {"Bar", "two"},
	//	}
	//
	// For incoming requests, the Host header is promoted to the
	// Request.Host field and removed from the Header map.
	//
	// HTTP defines that header names are case-insensitive. The
	// request parser implements this by using CanonicalHeaderKey,
	// making the first character and any characters following a
	// hyphen uppercase and the rest lowercase.
	//
	// For client requests, certain headers such as Content-Length
	// and Connection are automatically written when needed and
	// values in Header may be ignored. See the documentation
	// for the Request.Write method.
	Header Header

	// Body is the request's body.
	//
	// For client requests a nil body means the request has no
	// body, such as a GET request. The HTTP Client's Transport
	// is responsible for calling the Close method.
	//
	// For server requests the Request Body is always non-nil
	// but will return EOF immediately when no body is present.
	// The Server will close the request body. The ServeHTTP
	// Handler does not need to.
	Body io.ReadCloser

	// GetBody defines an optional func to return a new copy of
	// Body. It is used for client requests when a redirect requires
	// reading the body more than once. Use of GetBody still
	// requires setting Body.
	//
	// For server requests it is unused.
	GetBody func() (io.ReadCloser, error)

	// ContentLength records the length of the associated content.
	// The value -1 indicates that the length is unknown.
	// Values >= 0 indicate that the given number of bytes may
	// be read from Body.
	// For client requests, a value of 0 with a non-nil Body is
	// also treated as unknown.
	ContentLength int64

	// TransferEncoding lists the transfer encodings from outermost to
	// innermost. An empty list denotes the "identity" encoding.
	// TransferEncoding can usually be ignored; chunked encoding is
	// automatically added and removed as necessary when sending and
	// receiving requests.
	TransferEncoding []string

	// Close indicates whether to close the connection after
	// replying to this request (for servers) or after sending this
	// request and reading its response (for clients).
	//
	// For server requests, the HTTP server handles this automatically
	// and this field is not needed by Handlers.
	//
	// For client requests, setting this field prevents re-use of
	// TCP connections between requests to the same hosts, as if
	// Transport.DisableKeepAlives were set.
	Close bool

	// For server requests Host specifies the host on which the
	// URL is sought. Per RFC 2616, this is either the value of
	// the "Host" header or the host name given in the URL itself.
	// It may be of the form "host:port". For international domain
	// names, Host may be in Punycode or Unicode form. Use
	// golang.org/x/net/idna to convert it to either format if
	// needed.
	//
	// For client requests Host optionally overrides the Host
	// header to send. If empty, the Request.Write method uses
	// the value of URL.Host. Host may contain an international
	// domain name.
	Host string

	// Form contains the parsed form data, including both the URL
	// field's query parameters and the POST or PUT form data.
	// This field is only available after ParseForm is called.
	// The HTTP client ignores Form and uses Body instead.
	Form url.Values

	// PostForm contains the parsed form data from POST, PATCH,
	// or PUT body parameters.
	//
	// This field is only available after ParseForm is called.
	// The HTTP client ignores PostForm and uses Body instead.
	PostForm url.Values

	// MultipartForm is the parsed multipart form, including file uploads.
	// This field is only available after ParseMultipartForm is called.
	// The HTTP client ignores MultipartForm and uses Body instead.
	MultipartForm *multipart.Form

	// Trailer specifies additional headers that are sent after the request
	// body.
	//
	// For server requests the Trailer map initially contains only the
	// trailer keys, with nil values. (The client declares which trailers it
	// will later send.)  While the handler is reading from Body, it must
	// not reference Trailer. After reading from Body returns EOF, Trailer
	// can be read again and will contain non-nil values, if they were sent
	// by the client.
	//
	// For client requests Trailer must be initialized to a map containing
	// the trailer keys to later send. The values may be nil or their final
	// values. The ContentLength must be 0 or -1, to send a chunked request.
	// After the HTTP request is sent the map values can be updated while
	// the request body is read. Once the body returns EOF, the caller must
	// not mutate Trailer.
	//
	// Few HTTP clients, servers, or proxies support HTTP trailers.
	Trailer Header

	// RemoteAddr allows HTTP servers and other software to record
	// the network address that sent the request, usually for
	// logging. This field is not filled in by ReadRequest and
	// has no defined format. The HTTP server in this package
	// sets RemoteAddr to an "IP:port" address before invoking a
	// handler.
	// This field is ignored by the HTTP client.
	RemoteAddr string

	// RequestURI is the unmodified Request-URI of the
	// Request-Line (RFC 2616, Section 5.1) as sent by the client
	// to a server. Usually the URL field should be used instead.
	// It is an error to set this field in an HTTP client request.
	RequestURI string

	// TLS allows HTTP servers and other software to record
	// information about the TLS connection on which the request
	// was received. This field is not filled in by ReadRequest.
	// The HTTP server in this package sets the field for
	// TLS-enabled connections before invoking a handler;
	// otherwise it leaves the field nil.
	// This field is ignored by the HTTP client.
	TLS *tls.ConnectionState

	// Cancel is an optional channel whose closure indicates that the client
	// request should be regarded as canceled. Not all implementations of
	// RoundTripper may support Cancel.
	//
	// For server requests, this field is not applicable.
	//
	// Deprecated: Use the Context and WithContext methods
	// instead. If a Request's Cancel field and context are both
	// set, it is undefined whether Cancel is respected.
	Cancel <-chan struct{}

	// Response is the redirect response which caused this request
	// to be created. This field is only populated during client
	// redirects.
	Response *Response

	// ctx is either the client or server context. It should only
	// be modified via copying the whole Request using WithContext.
	// It is unexported to prevent people from using Context wrong
	// and mutating the contexts held by callers of the same request.
	ctx context.Context
}
```
Handler 需要知道关于请求的任何信息，都要从这个对象中获取，除非是做一些用户请求的修改等内容，一般不会直接修改这个对象。

ResponseWriter 是一个接口，定义了三个方法：
```golang
// A ResponseWriter interface is used by an HTTP handler to
// construct an HTTP response.
//
// A ResponseWriter may not be used after the Handler.ServeHTTP method
// has returned.
type ResponseWriter interface {
	// Header returns the header map that will be sent by
	// WriteHeader. The Header map also is the mechanism with which
	// Handlers can set HTTP trailers.
	//
	// Changing the header map after a call to WriteHeader (or
	// Write) has no effect unless the modified headers are
	// trailers.
	//
	// There are two ways to set Trailers. The preferred way is to
	// predeclare in the headers which trailers you will later
	// send by setting the "Trailer" header to the names of the
	// trailer keys which will come later. In this case, those
	// keys of the Header map are treated as if they were
	// trailers. See the example. The second way, for trailer
	// keys not known to the Handler until after the first Write,
	// is to prefix the Header map keys with the TrailerPrefix
	// constant value. See TrailerPrefix.
	//
	// To suppress implicit response headers (such as "Date"), set
	// their value to nil.
	Header() Header

	// Write writes the data to the connection as part of an HTTP reply.
	//
	// If WriteHeader has not yet been called, Write calls
	// WriteHeader(http.StatusOK) before writing the data. If the Header
	// does not contain a Content-Type line, Write adds a Content-Type set
	// to the result of passing the initial 512 bytes of written data to
	// DetectContentType.
	//
	// Depending on the HTTP protocol version and the client, calling
	// Write or WriteHeader may prevent future reads on the
	// Request.Body. For HTTP/1.x requests, handlers should read any
	// needed request body data before writing the response. Once the
	// headers have been flushed (due to either an explicit Flusher.Flush
	// call or writing enough data to trigger a flush), the request body
	// may be unavailable. For HTTP/2 requests, the Go HTTP server permits
	// handlers to continue to read the request body while concurrently
	// writing the response. However, such behavior may not be supported
	// by all HTTP/2 clients. Handlers should read before writing if
	// possible to maximize compatibility.
	Write([]byte) (int, error)

	// WriteHeader sends an HTTP response header with status code.
	// If WriteHeader is not called explicitly, the first call to Write
	// will trigger an implicit WriteHeader(http.StatusOK).
	// Thus explicit calls to WriteHeader are mainly used to
	// send error codes.
	WriteHeader(int)
}
```
Golang 中是没有类class 的概念，golang是通过 interface 类型接口实现的继承多态的效果。
interface就是一组抽象方法的集合，它必须由其他非interface类型实现，而不能自我实现， Go通过interface实现了duck-typing:即"当看到一只鸟走起来像鸭子、游泳起来像鸭子、叫起来也像鸭子，那么这只鸟就可以被称为鸭子"。

> 参考文档：
> - [golang_Interface](https://github.com/astaxie/build-web-application-with-golang/blob/master/zh/02.6.md)

我们看看这三个方法：
> - Header()：返回一个 Header 对象，可以通过它的 Set() 方法设置头部，注意最终返回的头部信息可能和你写进去的不完全相同，因为后续处理还可能修改头部的值（比如设置 Content-Length、Content-type 等操作）
> - Write()： 写 response 的主体部分，比如 html 或者 json 的内容就是放到这里的
> - WriteHeader()：设置 status code，如果没有调用这个函数，默认设置为 http.StatusOK，就是200状态码

实际上反馈的内容为：
```golang
// A response represents the server side of an HTTP response.
type response struct {
	conn             *conn
	req              *Request // request for this response
	reqBody          io.ReadCloser
	cancelCtx        context.CancelFunc // when ServeHTTP exits
	wroteHeader      bool               // reply header has been (logically) written
	wroteContinue    bool               // 100 Continue response was written
	wants10KeepAlive bool               // HTTP/1.0 w/ Connection "keep-alive"
	wantsClose       bool               // HTTP request has Connection "close"

	w  *bufio.Writer // buffers output in chunks to chunkWriter
	cw chunkWriter

	// handlerHeader is the Header that Handlers get access to,
	// which may be retained and mutated even after WriteHeader.
	// handlerHeader is copied into cw.header at WriteHeader
	// time, and privately mutated thereafter.
	handlerHeader Header
	calledHeader  bool // handler accessed handlerHeader via Header

	written       int64 // number of bytes written in body
	contentLength int64 // explicitly-declared Content-Length; or -1
	status        int   // status code passed to WriteHeader

	// close connection after this reply.  set on request and
	// updated after response from handler if there's a
	// "Connection: keep-alive" response header and a
	// Content-Length.
	closeAfterReply bool

	// requestBodyLimitHit is set by requestTooLarge when
	// maxBytesReader hits its max size. It is checked in
	// WriteHeader, to make sure we don't consume the
	// remaining request body to try to advance to the next HTTP
	// request. Instead, when this is set, we stop reading
	// subsequent requests on this connection and stop reading
	// input from it.
	requestBodyLimitHit bool

	// trailers are the headers to be sent after the handler
	// finishes writing the body. This field is initialized from
	// the Trailer response header when the response header is
	// written.
	trailers []string

	handlerDone atomicBool // set true when the handler exits

	// Buffers for Date and Content-Length
	dateBuf [len(TimeFormat)]byte
	clenBuf [10]byte

	// closeNotifyCh is the channel returned by CloseNotify.
	// TODO(bradfitz): this is currently (for Go 1.8) always
	// non-nil. Make this lazily-created again as it used to be?
	closeNotifyCh  chan bool
	didCloseNotify int32 // atomic (only 0->1 winner should send)
}
```
实现了上面提到的三个方法，所以可以作为参数交给Handler作为参数使用。



## 整体执行流程总结：
对http包的分析之后，现在让我们来梳理一下整个的代码执行过程：

### 首先调用Http.HandleFunc，按顺序做了几件事：
> - 1 调用了DefaultServeMux的HandleFunc
> - 2 调用了DefaultServeMux的Handle
> - 3 往DefaultServeMux的map[string]muxEntry中增加对应的handler和路由规则

### 其次调用http.ListenAndServe(":9090", nil)，按顺序做了几件事情：

> - 1 实例化Server
> - 2 调用Server的ListenAndServe()
> - 3 调用net.Listen("tcp", addr)监听端口
> - 4 启动一个for循环，在循环体中Accept请求
> - 5 对每个请求实例化一个Conn，并且开启一个goroutine为这个请求进行服务go c.serve()
> - 6 读取每个请求的内容w, err := c.readRequest()
> - 7 判断handler是否为空，如果没有设置handler（这个例子就没有设置handler），handler就设置为DefaultServeMux
> - 8 调用handler的ServeHttp
> - 9 在这个例子中，下面就进入到DefaultServeMux.ServeHttp
> - 10 根据request选择handler，并且进入到这个handler的ServeHTTP：mux.handler(r).ServeHTTP(w, r)
> - 11 选择handler：<br> A 判断是否有路由能满足这个request（循环遍历ServerMux的muxEntry）<br> B 如果有路由满足，调用这个路由handler的ServeHttp <br> C 如果没有路由满足，调用NotFoundHandler的ServeHttp

## 一些使用细节：
设置response的返回类型：
[HTTP Response Snippets for Go](http://www.alexedwards.net/blog/golang-response-snippets)

json转换为字符串的正则替换：
" 替换为 \"
[ \n]+ 替换为空

if else结构中，if和else之间的花括号中不能插入注释，否则无法识别。
```golang
if err != nil {
    fmt.Println("wrong password")
} else {
    //接下来就可以判断这个数字的大小范围了
    fmt.Println(getint)
}
```

golang中使用了re2作为正则表达式的引擎：[RE2](https://github.com/google/re2)，并且默认支持UTF-8编码。
golang提供了对中文的良好支持，对于中文的检测可以使用如下正则：
```golang
m, _ := regexp.MatchString("^\\p{Han}+$", r.Form.Get("realname"))
```




