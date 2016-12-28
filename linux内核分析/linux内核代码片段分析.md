# Linux内核代码片段分析：

本文结合GCC针对ISO C的扩展，对于Linux内核中的典型代码片段进行分析和梳理。

## 1 标准C语言的GCC扩展
Linux 完全依靠 GCC 在新的体系结构上运行。Linux 还利用 GCC 中的特性（称为扩展）实现更多功能和优化。所以在查看linux源代码的时候会遇到非常多的GCC扩展情况。这一节主要介绍GCC对于ISO C的一些扩展。

本节的主要内容参考了 [Linux 内核中的 GCC 特性](https://www.ibm.com/developerworks/cn/linux/l-gcc-hacks/) 这篇文章。

GCC支持很多的C语言规范，具体的支持情况可以通过官方的对应版本文档进行查看。以4.8.5版本为例，在 [官方文档](https://gcc.gnu.org/onlinedocs/gcc-4.8.5/cpp/Invocation.html#Invocation) 中说明：
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

> 优化：扩展帮助生成更高效的代码。
  
### 1.1 功能性扩展：
这一节主要分析GCC对于ISO C的功能上的扩展，相当于添加了新的语法特性。

#### 1.1.1 类型发现：
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

#### 1.1.2 范围：
GCC 支持范围，在 C 语言的许多方面都可以使用范围。在复杂的条件结构中，通常依靠嵌套的if语句实现多条件的判断，使用GCC扩展的范围之后可以简化代码。

##### 1.1.2.1 在switch/case语句中使用范围：
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

##### 1.1.2.2 使用范围进行初始化：
除了在控制语句中使用范围指定整体，范围还可以应用在变量的初始化中来简化代码。
源代码位置：
./linux/arch/cris/arch-v32/kernel/smp.c
```gcc-c
/* Vector of locks used for various atomic operations */
spinlock_t cris_atomic_locks[] = { [0 ... LOCK_COUNT - 1] = SPIN_LOCK_UNLOCKED};
```
在这个例子中，为 spinlock_t 创建一个大小为 LOCK_COUNT 的数组。数组的每个元素初始化为 SPIN_LOCK_UNLOCKED 值。
范围还支持更复杂的初始化。例如，以下代码指定数组中几个子范围的初始值：
```gcc-c
int widths[] = { [0 ... 9] = 1, [10 ... 99] = 2, [100] = 3 };
```

#### 1.1.3 零长度的数组：
在 C 标准中，必须定义至少一个数组元素。这个需求往往会使代码设计复杂化。但是，GCC 支持零长度数组的概念，这对于结构定义尤其有用。这个概念与 ISO C99 中灵活的数组成员相似，但是使用不同的语法。
源代码位置：./linux/drivers/ieee1394/raw1394-private.h
```gcc-c
struct iso_block_store {
        atomic_t refcount;
        size_t data_size;
        quadlet_t data[0];
};
```
这个结构体定义中，在结构的末尾声明一个没有成员的数组data，这允许结构中的元素引用结构实例后面紧接着的内存。在需要数量可变的数组成员时，这个特性很有用。

#### 1.1.4 判断调用地址：


### 1.2 功能性扩展：
#### 1.2.1
##### 1.2.1.1




## 2 Linux kernel经典代码分析：

### 2.1 链表：
在linux内核的驱动开发中，链表有着重要的作用。内核中的链表实现位于：
```shell
../include/linux/list.h
```
链表的基本结构很简单：
```shell

```












