armv7l 搭建开发环境：

首先卸载默认安装的python
sudo apt-get --purge remove python*
然后使用anaconda替换：
访问：https://repo.continuum.io/miniconda/
选中：Miniconda3-latest-Linux-armv7l.sh
sudo apt-get install wget curl
cd /home/android
wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-armv7l.sh
sudo md5sum Miniconda3-latest-Linux-armv7l.sh
sudo /bin/bash Miniconda3-latest-Linux-armv7l.sh
输入回车，yes确认后，输入安装路径地址，完成安装。

然后设置当前安装的python作为系统默认的python版本，添加Python2可选项，优先级为1：
update-alternatives --display python
sudo update-alternatives --install /usr/bin/python python /usr/local/miniconda3/bin/python3.4 1
再次查看当前系统中设置的python：
update-alternatives --display python

这个时候在任何目录下输入python就可以了。

然后还可以将当前的路径添加到环境变量，这样就可以使用conda来进行管理了。
sudo ./conda update --all
sudo ./conda install tensorflow
无法安装tensorflow，这部分可以参考：
[Tensorflow on Raspberry Pi](https://github.com/tensorflow/tensorflow/issues/254)
[Installing TensorFlow on Raspberry Pi 3 (and probably 2 as well)](https://github.com/samjabrahams/tensorflow-on-raspberry-pi)

可以通过编译的方式完成支持。


安装必要的开发软件：
sudo apt-get install build-essential gcc cmake pkg-config git


如果需要安装视频相关的操作
音訊視訊的編解碼、錄製、轉換、串流：
$ sudo apt-get install libavcodec-dev libavformat-dev libswscale-dev

圖檔格式（選用性）：
$ sudo apt-get install libjpeg-dev libpng-dev libtiff-dev libjasper-dev


然后安装oracle的jdk：


编译安装golang：
需要bootstrap，首先从官方下载已经有的arm版本编译器：
https://golang.org/dl/
或者从第三方下载提供的交叉编译器：
curl http://dave.cheney.net/paste/go-linux-arm-bootstrap-c788a8e.tbz | tar xj


然后获取需要版本的代码：
git clone

接着执行编译：
ulimit -s 1024     # set the thread stack limit to 1mb
ulimit -s          # check that it worked
cd src
env GO_TEST_TIMEOUT_SCALE=10 GOROOT_BOOTSTRAP=/home/android/go-linux-arm-bootstrap ./all.bash


编译执行为：
```shell
android@localhost:~/go-go1.9rc1/src$ env GO_TEST_TIMEOUT_SCALE=10 GOROOT_BOOTSTRAP=/home/android/go-linux-arm-bootstrap ./all.bash
##### Building Go bootstrap tool.
cmd/dist

##### Building Go toolchain using /home/android/go-linux-arm-bootstrap.
bootstrap/cmd/internal/sys
bootstrap/cmd/internal/src
bootstrap/cmd/internal/objabi
bootstrap/cmd/internal/dwarf
bootstrap/cmd/asm/internal/flags
bootstrap/cmd/internal/bio
bootstrap/math/bits
bootstrap/cmd/internal/gcprog
bootstrap/debug/pe
bootstrap/math/big
bootstrap/cmd/compile/internal/syntax
bootstrap/cmd/internal/obj
bootstrap/cmd/asm/internal/lex
bootstrap/cmd/link/internal/ld
bootstrap/cmd/internal/obj/arm
bootstrap/cmd/internal/obj/mips
bootstrap/cmd/internal/obj/arm64
bootstrap/cmd/internal/obj/ppc64
bootstrap/cmd/internal/obj/s390x
bootstrap/cmd/internal/obj/x86
bootstrap/cmd/compile/internal/types
bootstrap/cmd/asm/internal/arch
bootstrap/cmd/compile/internal/ssa
bootstrap/cmd/asm/internal/asm
bootstrap/cmd/asm
bootstrap/cmd/link/internal/amd64
bootstrap/cmd/link/internal/arm
bootstrap/cmd/link/internal/arm64
bootstrap/cmd/link/internal/mips
bootstrap/cmd/link/internal/mips64
bootstrap/cmd/link/internal/ppc64
bootstrap/cmd/link/internal/s390x
bootstrap/cmd/link/internal/x86
bootstrap/cmd/link
bootstrap/cmd/compile/internal/gc
bootstrap/cmd/compile/internal/amd64
bootstrap/cmd/compile/internal/arm
bootstrap/cmd/compile/internal/arm64
bootstrap/cmd/compile/internal/mips
bootstrap/cmd/compile/internal/mips64
bootstrap/cmd/compile/internal/ppc64
bootstrap/cmd/compile/internal/s390x
bootstrap/cmd/compile/internal/x86
bootstrap/cmd/compile

##### Building go_bootstrap for host, linux/arm.
runtime/internal/sys
runtime/internal/atomic
runtime
math/bits
unicode/utf8
sync/atomic
unicode/utf16
unicode
internal/cpu
internal/syscall/windows/sysdll
errors
encoding
internal/race
math
sync
syscall
io
internal/singleflight
cmd/go/internal/web
hash
strings
hash/adler32
bytes
strconv
bufio
path
encoding/base64
crypto
reflect
crypto/sha1
internal/syscall/windows/registry
internal/syscall/windows
time
internal/poll
os
sort
encoding/binary
os/signal
fmt
path/filepath
container/heap
regexp/syntax
io/ioutil
text/template/parse
debug/dwarf
net/url
encoding/xml
context
log
cmd/go/internal/str
flag
compress/flate
go/token
encoding/json
regexp
go/scanner
os/exec
cmd/internal/objabi
go/ast
compress/zlib
text/template
debug/elf
debug/macho
go/parser
go/doc
go/build
cmd/go/internal/cfg
cmd/go/internal/buildid
cmd/go/internal/base
cmd/go/internal/doc
cmd/go/internal/load
cmd/go/internal/help
cmd/go/internal/tool
cmd/go/internal/cmdflag
cmd/go/internal/version
cmd/go/internal/fmtcmd
cmd/go/internal/work
cmd/go/internal/fix
cmd/go/internal/envcmd
cmd/go/internal/test
cmd/go/internal/clean
cmd/go/internal/list
cmd/go/internal/get
cmd/go/internal/vet
cmd/go/internal/run
cmd/go/internal/generate
cmd/go/internal/bug
cmd/go

##### Building packages and commands for linux/arm.
runtime/internal/sys
runtime/internal/atomic
runtime
errors
internal/race
sync/atomic
unicode
unicode/utf8
math
container/list
math/bits
container/ring
crypto/subtle
crypto/internal/cipherhw
internal/nettrace
encoding
vendor/golang_org/x/crypto/poly1305
unicode/utf16
image/color
sync
internal/cpu
internal/syscall/windows
internal/syscall/windows/registry
internal/syscall/windows/sysdll
runtime/race
vendor/golang_org/x/text/secure
vendor/golang_org/x/text/unicode
cmd/compile/internal/test
cmd/vet/internal/whitelist
image/color/palette
io
syscall
internal/singleflight
crypto/cipher
hash
runtime/trace
hash/adler32
hash/crc32
crypto/hmac
hash/crc64
hash/fnv
bytes
strings
math/rand
strconv
math/cmplx
bufio
text/tabwriter
vendor/golang_org/x/text/transform
path
html
time
internal/syscall/unix
cmd/internal/src
crypto/aes
reflect
crypto
crypto/rc4
encoding/base64
encoding/ascii85
encoding/base32
crypto/sha512
crypto/md5
crypto/sha1
crypto/sha256
image
image/internal/imageutil
internal/poll
image/draw
image/jpeg
os
os/signal
fmt
sort
encoding/binary
regexp/syntax
encoding/pem
container/heap
path/filepath
compress/bzip2
runtime/debug
cmd/internal/sys
crypto/des
vendor/golang_org/x/crypto/chacha20poly1305/internal/chacha20
vendor/golang_org/x/crypto/curve25519
vendor/golang_org/x/crypto/chacha20poly1305
io/ioutil
flag
log
debug/dwarf
compress/flate
debug/gosym
debug/plan9obj
cmd/vendor/golang.org/x/arch/arm/armasm
cmd/vendor/golang.org/x/arch/x86/x86asm
cmd/vendor/golang.org/x/arch/ppc64/ppc64asm
regexp
archive/tar
cmd/internal/objabi
compress/zlib
cmd/internal/goobj
archive/zip
compress/gzip
compress/lzw
context
debug/elf
debug/macho
debug/pe
math/big
encoding/hex
go/token
os/exec
database/sql/driver
go/scanner
encoding/csv
encoding/gob
encoding/json
encoding/xml
go/ast
database/sql
cmd/internal/objfile
vendor/golang_org/x/net/http2/hpack
vendor/golang_org/x/text/unicode/bidi
vendor/golang_org/x/text/unicode/norm
net/url
vendor/golang_org/x/text/secure/bidirule
crypto/dsa
crypto/elliptic
encoding/asn1
cmd/addr2line
crypto/rand
go/parser
go/printer
mime
crypto/x509/pkix
mime/quotedprintable
crypto/rsa
crypto/ecdsa
net/http/internal
text/template/parse
go/constant
text/scanner
image/gif
vendor/golang_org/x/net/idna
image/png
index/suffixarray
cmd/cgo
go/format
go/types
testing
internal/trace
runtime/pprof
text/template
net/internal/socktest
runtime/pprof/internal/profile
internal/testenv
testing/iotest
testing/quick
testing/internal/testdeps
cmd/internal/dwarf
cmd/asm/internal/flags
cmd/internal/bio
cmd/compile/internal/syntax
cmd/asm/internal/lex
cmd/internal/obj
cmd/internal/gcprog
go/doc
html/template
cmd/internal/browser
cmd/dist
cmd/fix
go/build
cmd/internal/obj/arm
cmd/internal/obj/arm64
go/internal/gccgoimporter
cmd/internal/obj/mips
runtime/cgo
cmd/internal/obj/ppc64
go/internal/gcimporter
go/internal/srcimporter
cmd/api
cmd/internal/obj/s390x
cmd/internal/obj/x86
cmd/compile/internal/types
go/importer
cmd/cover
cmd/doc
cmd/go/internal/cfg
cmd/go/internal/str
cmd/gofmt
cmd/go/internal/base
net
os/user
plugin
cmd/asm/internal/arch
cmd/compile/internal/ssa
cmd/go/internal/buildid
cmd/asm/internal/asm
cmd/go/internal/doc
cmd/go/internal/help
cmd/go/internal/cmdflag
cmd/go/internal/tool
cmd/go/internal/version
cmd/link/internal/ld
cmd/nm
cmd/go/internal/load
cmd/objdump
cmd/pack
cmd/asm
cmd/go/internal/work
cmd/go/internal/fix
cmd/go/internal/fmtcmd
cmd/vendor/github.com/google/pprof/internal/elfexec
cmd/vendor/github.com/google/pprof/profile
cmd/vendor/github.com/ianlancetaylor/demangle
cmd/vendor/github.com/google/pprof/third_party/svg
cmd/vendor/github.com/google/pprof/internal/proftest
cmd/vet/internal/cfg
cmd/vet
crypto/x509
vendor/golang_org/x/net/lex/httplex
vendor/golang_org/x/net/proxy
net/textproto
log/syslog
vendor/golang_org/x/net/nettest
cmd/go/internal/envcmd
mime/multipart
crypto/tls
net/mail
cmd/go/internal/clean
cmd/go/internal/generate
cmd/go/internal/list
cmd/go/internal/run
cmd/go/internal/test
cmd/go/internal/vet
cmd/link/internal/amd64
cmd/link/internal/arm
cmd/link/internal/arm64
cmd/link/internal/mips
cmd/link/internal/mips64
cmd/link/internal/ppc64
cmd/link/internal/s390x
cmd/link/internal/x86
cmd/vendor/github.com/google/pprof/internal/plugin
cmd/vendor/github.com/google/pprof/internal/measurement
cmd/vendor/github.com/google/pprof/internal/binutils
cmd/vendor/github.com/google/pprof/internal/symbolz
cmd/link
cmd/vendor/github.com/google/pprof/internal/graph
net/http/httptrace
net/smtp
net/http
cmd/vendor/github.com/google/pprof/internal/report
expvar
net/http/httputil
net/http/cookiejar
net/http/httptest
net/http/pprof
net/http/cgi
net/rpc
cmd/go/internal/web
cmd/vendor/github.com/google/pprof/internal/symbolizer
cmd/trace
net/http/fcgi
cmd/go/internal/bug
cmd/go/internal/get
net/rpc/jsonrpc
cmd/vendor/github.com/google/pprof/internal/driver
cmd/go
cmd/vendor/github.com/google/pprof/driver
cmd/pprof
cmd/compile/internal/gc
cmd/compile/internal/amd64
cmd/compile/internal/mips
cmd/compile/internal/arm64
cmd/compile/internal/mips64
cmd/compile/internal/arm
cmd/compile/internal/ppc64
cmd/compile/internal/s390x
cmd/compile/internal/x86
cmd/compile


##### cmd/go terminal test
PASS
ok      _/home/android/go-go1.9rc1/src/cmd/go/testdata/testterminal18153        0.014s

##### Testing packages.
ok      archive/tar     0.466s
ok      archive/zip     9.676s
ok      bufio   0.524s
ok      bytes   5.821s
ok      compress/bzip2  0.414s
ok      compress/flate  8.395s
ok      compress/gzip   0.206s
ok      compress/lzw    0.124s
ok      compress/zlib   0.408s
ok      container/heap  0.053s
ok      container/list  0.071s
ok      container/ring  0.115s
ok      context 1.076s
ok      crypto/aes      0.208s
ok      crypto/cipher   0.072s
ok      crypto/des      0.070s
ok      crypto/dsa      0.066s
ok      crypto/ecdsa    0.544s
ok      crypto/elliptic 0.190s
ok      crypto/hmac     0.044s
ok      crypto/md5      0.050s
ok      crypto/rand     0.544s
ok      crypto/rc4      0.500s
ok      crypto/rsa      0.870s
ok      crypto/sha1     0.099s
ok      crypto/sha256   0.048s
ok      crypto/sha512   0.097s
ok      crypto/subtle   0.070s
ok      crypto/tls      9.059s
ok      crypto/x509     10.257s
ok      database/sql    1.347s
ok      database/sql/driver     0.032s
ok      debug/dwarf     0.146s
ok      debug/elf       0.188s
ok      debug/gosym     0.073s
ok      debug/macho     0.048s
ok      debug/pe        0.091s
ok      debug/plan9obj  0.063s
ok      encoding/ascii85        0.090s
ok      encoding/asn1   0.067s
ok      encoding/base32 0.088s
ok      encoding/base64 0.079s
ok      encoding/binary 0.048s
ok      encoding/csv    0.066s
ok      encoding/gob    0.489s
ok      encoding/hex    0.062s
ok      encoding/json   2.944s
ok      encoding/pem    0.081s
ok      encoding/xml    0.228s
ok      errors  0.051s
ok      expvar  0.066s
ok      flag    0.051s
ok      fmt     0.520s
ok      go/ast  0.122s
ok      go/build        1.134s
ok      go/constant     0.058s
ok      go/doc  0.515s
ok      go/format       0.093s
ok      go/internal/gccgoimporter       0.085s
ok      go/internal/gcimporter  3.613s
ok      go/internal/srcimporter 4.857s
ok      go/parser       0.247s
ok      go/printer      2.657s
ok      go/scanner      0.057s
ok      go/token        0.243s
ok      go/types        5.700s
ok      hash/adler32    0.079s
ok      hash/crc32      0.065s
ok      hash/crc64      0.042s
ok      hash/fnv        0.052s
ok      html    0.129s
ok      html/template   0.280s
ok      image   0.567s
ok      image/color     0.131s
ok      image/draw      0.352s
ok      image/gif       0.649s
ok      image/jpeg      1.414s
ok      image/png       0.342s
ok      index/suffixarray       0.143s
ok      internal/cpu    0.072s
ok      internal/poll   0.128s
ok      internal/singleflight   0.090s
ok      internal/trace  5.025s
ok      io      0.112s
ok      io/ioutil       0.075s
ok      log     0.052s
ok      log/syslog      1.349s
ok      math    0.067s
ok      math/big        14.807s
ok      math/bits       0.050s
ok      math/cmplx      0.048s
ok      math/rand       0.677s
ok      mime    0.129s
ok      mime/multipart  4.158s
ok      mime/quotedprintable    1.687s
ok      net     5.527s
ok      net/http        17.240s
ok      net/http/cgi    2.572s
ok      net/http/cookiejar      0.101s
ok      net/http/fcgi   0.073s
ok      net/http/httptest       0.313s
ok      net/http/httptrace      0.097s
ok      net/http/httputil       0.573s
ok      net/http/internal       0.042s
ok      net/internal/socktest   0.082s
ok      net/mail        0.077s
ok      net/rpc 0.155s
ok      net/rpc/jsonrpc 0.086s
ok      net/smtp        0.232s
ok      net/textproto   0.072s
ok      net/url 0.092s
ok      os      1.222s
ok      os/exec 3.331s
ok      os/signal       5.345s
ok      os/user 0.047s
ok      path    0.042s
ok      path/filepath   0.250s
ok      reflect 2.289s
ok      regexp  1.264s
ok      regexp/syntax   3.539s
ok      runtime 95.426s
ok      runtime/debug   0.202s
ok      runtime/internal/atomic 2.196s
ok      runtime/internal/sys    0.059s
ok      runtime/pprof   9.093s
ok      runtime/pprof/internal/profile  0.043s
ok      runtime/trace   19.537s
ok      sort    0.359s
ok      strconv 2.177s
ok      strings 3.022s
ok      sync    2.582s
ok      sync/atomic     0.714s
ok      syscall 0.238s
ok      testing 1.732s
ok      testing/quick   1.183s
ok      text/scanner    0.058s
ok      text/tabwriter  0.085s
ok      text/template   1.730s
ok      text/template/parse     0.125s
ok      time    4.532s
ok      unicode 0.029s
ok      unicode/utf16   0.043s
ok      unicode/utf8    0.081s
ok      vendor/golang_org/x/crypto/chacha20poly1305     1.516s
ok      vendor/golang_org/x/crypto/chacha20poly1305/internal/chacha20   0.044s
ok      vendor/golang_org/x/crypto/curve25519   0.801s
ok      vendor/golang_org/x/crypto/poly1305     0.045s
ok      vendor/golang_org/x/net/http2/hpack     0.042s
ok      vendor/golang_org/x/net/idna    0.054s
ok      vendor/golang_org/x/net/lex/httplex     0.057s
ok      vendor/golang_org/x/net/nettest 2.873s
ok      vendor/golang_org/x/net/proxy   0.080s
ok      vendor/golang_org/x/text/transform      0.048s
ok      vendor/golang_org/x/text/unicode/norm   0.068s
ok      cmd/addr2line   11.021s
ok      cmd/api 0.076s
ok      cmd/asm/internal/asm    2.575s
ok      cmd/asm/internal/lex    0.050s
ok      cmd/compile     35.256s
ok      cmd/compile/internal/gc 56.811s
ok      cmd/compile/internal/ssa        1.694s
ok      cmd/compile/internal/syntax     0.062s
ok      cmd/compile/internal/test       0.055s [no tests to run]
ok      cmd/compile/internal/types      0.048s
ok      cmd/cover       10.768s
ok      cmd/doc 0.528s
ok      cmd/fix 0.131s
ok      cmd/go  22.918s
ok      cmd/go/internal/generate        0.061s
ok      cmd/go/internal/get     0.123s
ok      cmd/go/internal/load    0.053s
ok      cmd/go/internal/work    0.066s
ok      cmd/gofmt       0.579s
ok      cmd/internal/dwarf      0.047s
ok      cmd/internal/obj        0.051s
ok      cmd/internal/obj/arm64  0.051s
ok      cmd/internal/obj/x86    19.071s
ok      cmd/internal/objabi     0.047s
ok      cmd/internal/src        0.052s
ok      cmd/link        29.901s
ok      cmd/link/internal/ld    25.205s
ok      cmd/nm  23.908s
ok      cmd/objdump     16.903s
ok      cmd/pack        14.135s
ok      cmd/trace       0.105s
ok      cmd/vendor/github.com/google/pprof/internal/binutils    0.061s
ok      cmd/vendor/github.com/google/pprof/internal/driver      2.910s
ok      cmd/vendor/github.com/google/pprof/internal/elfexec     0.047s
ok      cmd/vendor/github.com/google/pprof/internal/graph       0.069s
ok      cmd/vendor/github.com/google/pprof/internal/report      0.064s
ok      cmd/vendor/github.com/google/pprof/internal/symbolizer  0.080s
ok      cmd/vendor/github.com/google/pprof/internal/symbolz     0.050s
ok      cmd/vendor/github.com/google/pprof/profile      1.056s
ok      cmd/vendor/github.com/ianlancetaylor/demangle   0.190s
ok      cmd/vendor/golang.org/x/arch/arm/armasm 0.068s
ok      cmd/vendor/golang.org/x/arch/ppc64/ppc64asm     0.068s
ok      cmd/vendor/golang.org/x/arch/x86/x86asm 0.584s
ok      cmd/vet 13.543s
ok      cmd/vet/internal/cfg    0.046s

##### GOMAXPROCS=2 runtime -cpu=1,2,4
ok      runtime 150.556s

##### sync -cpu=10
ok      sync    1.360s

##### ../misc/cgo/stdio

##### ../misc/cgo/life

##### ../misc/cgo/test
PASS
ok      _/home/android/go-go1.9rc1/misc/cgo/test        30.663s
PASS
ok      _/home/android/go-go1.9rc1/misc/cgo/test        30.267s
PASS
ok      _/home/android/go-go1.9rc1/misc/cgo/test        30.502s
PASS
ok      _/home/android/go-go1.9rc1/misc/cgo/testtls     0.030s
PASS
ok      _/home/android/go-go1.9rc1/misc/cgo/testtls     0.014s
PASS
ok      _/home/android/go-go1.9rc1/misc/cgo/testtls     0.022s
PASS
ok      _/home/android/go-go1.9rc1/misc/cgo/nocgo       0.024s
PASS
ok      _/home/android/go-go1.9rc1/misc/cgo/nocgo       0.017s
PASS
ok      _/home/android/go-go1.9rc1/misc/cgo/nocgo       0.056s
PASS
ok      _/home/android/go-go1.9rc1/misc/cgo/test        24.197s
PASS
ok      _/home/android/go-go1.9rc1/misc/cgo/testtls     0.024s
PASS
ok      _/home/android/go-go1.9rc1/misc/cgo/nocgo       0.049s

##### ../misc/cgo/testgodefs

##### ../misc/cgo/testso

##### ../misc/cgo/testsovar

##### ../misc/cgo/testcshared
ok

##### ../misc/cgo/testshared
PASS
ok      _/home/android/go-go1.9rc1/misc/cgo/testshared  111.004s

##### ../misc/cgo/testplugin
PASS
something

##### ../misc/cgo/errors

##### ../misc/cgo/testsigfwd

##### ../test/bench/go1
testing: warning: no tests to run
PASS
ok      _/home/android/go-go1.9rc1/test/bench/go1       5.415s

##### ../test

##### API check
Go version is "go1.9rc1", ignoring -next /home/android/go-go1.9rc1/api/next.txt

ALL TESTS PASSED

---
Installed Go for linux/arm in /home/android/go-go1.9rc1
Installed commands in /home/android/go-go1.9rc1/bin
*** You need to add /home/android/go-go1.9rc1/bin to your PATH.
```
对比PC上的编译，还是非常耗时的。


参考：
[Building Go 1.5 on the Raspberry Pi](https://dave.cheney.net/2015/09/04/building-go-1-5-on-the-raspberry-pi)
