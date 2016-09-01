这个文章主要涉及了linux内核源代码中相关的代码片段，然后进行分析和梳理。

 Linux 完全依靠 GCC 在新的体系结构上运行。Linux 还利用 GCC 中的特性（称为扩展）实现更多功能和优化。所以在查看linux源代码的时候会遇到非常多的GCC扩展情况，

# 1 标准C语言的GCC扩展
GCC支持很多的C语言规范，具体的支持情况可以通过官方的对应版本文档进行查看。以4.8.5版本为例，[官方文档](https://gcc.gnu.org/onlinedocs/gcc-4.8.5/cpp/Invocation.html#Invocation)，中说明：
```shell
-std=standard
-ansi
	Specify the standard to which the code should conform. Currently CPP knows about C and C++ standards; others may be added in the future.
	standard may be one of:

	c90
	c89
	iso9899:1990
		The ISO C standard from 1990. ‘c90’ is the customary shorthand for this version of the standard.
		The -ansi option is equivalent to -std=c90. 

	iso9899:199409
		The 1990 C standard, as amended in 1994. 
	iso9899:1999
	c99
	iso9899:199x
	c9x
		The revised ISO C standard, published in December 1999. Before publication, this was known as C9X. 
	iso9899:2011
	c11
	c1x
		The revised ISO C standard, published in December 2011. Before publication, this was known as C1X. 
	gnu90
	gnu89
		The 1990 C standard plus GNU extensions. This is the default. 
	gnu99
	gnu9x
		The 1999 C standard plus GNU extensions. 
	gnu11
	gnu1x
		The 2011 C standard plus GNU extensions. 
	c++98
		The 1998 ISO C++ standard plus amendments. 
	gnu++98
		The same as -std=c++98 plus GNU extensions. This is the default for C++ code.
```
可以看出当前版本的编译器支持的所有具体版本信息。

本文假设C语言使用 ISO C99 标准，GCC对于标准C进行了扩展，主要有两个方面的内容：
> 功能性：扩展提供新功能。
  优化：扩展帮助生成更高效的代码。
  
本文结合linux源代码对常用的扩展结合进行分析。

## 1.1 类型发现：
GCC允许通过变量的引用识别类型。这种操作支持泛型编程。使用typeof构建一个泛型宏：
源代码位置： ./linux/include/linux/kernel.h
```gcc-c
#define min(x, y)                                 /
({                                                /
    typeof(x) _min1 = (x);                        /
    typeof(y) _min2 = (y);                        /
    (void) (&_min1 == &_min2);                    /
    _min1 < _min2 ? _min1 : _min2;                /
}) 
```
这个宏中的"(void) (&_min1 == &_min2);"这段代码是为了检查是否是同类型的变量进行比较，如果没有这一句判断，就无法发出如下警告：
```shell
comparison of distinct pointer types lacks a cast
```

## 1.2 范围：
GCC 支持范围，在 C 语言的许多方面都可以使用范围。在复杂的条件结构中，通常依靠嵌套的if语句实现多条件的判断，使用GCC扩展的范围之后可以简化代码。
例如在 switch/case 块中的 case 语句中使用范围。同事，使用 switch/case 也可以通过使用跳转表实现进行编译器优化。
源代码位置： ./linux/drivers/scsi/sd.c
```gcc-c
static int sd_major(int major_idx)
{
	switch (major_idx) {
	case 0:
		return SCSI_DISK0_MAJOR;
	case 1 ... 7:
		return SCSI_DISK1_MAJOR + major_idx - 1;
	case 8 ... 15:
		return SCSI_DISK8_MAJOR + major_idx - 8;
	default:
		BUG();
		return 0;	/* shut up gcc */
	}
}
```
其中"case 1 ... 7:"使用三个点来表示从1到7的整个范围，简化了逐一的条件判断。
