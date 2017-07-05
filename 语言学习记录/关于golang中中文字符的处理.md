关于golang中中文字符的处理：
Go里面内建仅支持UTF8字符串编码，部分中文采用的GBK或者GB2312编码就不能正确显示了。
所以需要从对应字符串的字节流进行转换才能看到，为了做这个转码工作，采用了github.com/axgle/mahonia这个库：
go get github.com/axgle/mahonia
```golang
package main

import (
    "fmt"
    "github.com/axgle/mahonia"
)

func main(){
    // 将UTF-8转换为 GBK
    gbk := mahonia.NewEncoder("gbk").ConvertString("hello,世界")
    fmt.Println(gbk)


    // "你好，世界！"的GBK编码
    gbkBytes := []byte{0xC4, 0xE3, 0xBA, 0xC3, 0xA3, 0xAC, 0xCA, 0xC0, 0xBD, 0xE7, 0xA3, 0xA1}

    // 将GBK转换为UTF-8
    utf8 := mahonia.NewDecoder("gbk").ConvertString(string(gbkBytes))
    fmt.Println(utf8)
}
```
以os包的exec执行结果为例：
```golang
func pingIP() {
	cmd := exec.Command("ping", "127.0.0.1")
	out, err := cmd.Output()
	if err != nil {
		println("Command Error!", err.Error())
		return
	}
	// 直接输出string转换的字节流会乱码
	//fmt.Println(string(out))
	// out是字节流，需要转码为UTF-8才能在golang中正确显示，使用了"github.com/axgle/mahonia"这个库进行字节的编码转换
	utf8 := mahonia.NewDecoder("gbk").ConvertString(string(out))
	fmt.Println(utf8)
}
```