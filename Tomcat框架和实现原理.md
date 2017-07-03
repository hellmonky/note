<!-- TOC -->

- [Tomcat框架和实现原理](#tomcat框架和实现原理)
    - [一些反思：](#一些反思)
    - [Tomcat的编译和测试：](#tomcat的编译和测试)
    - [Tomcat的功能描述：](#tomcat的功能描述)
    - [Tomcat的启动逻辑：](#tomcat的启动逻辑)
        - [从启动脚本开始：](#从启动脚本开始)
        - [Tomcat的启动入口代码：](#tomcat的启动入口代码)
            - [实例化Boostrap对象：](#实例化boostrap对象)
            - [Boostrap调用init初始化方法：](#boostrap调用init初始化方法)
                - [初始化类加载器：](#初始化类加载器)

<!-- /TOC -->


# Tomcat框架和实现原理

java web开发始终离不开servlet容器，其中最出名的就是Apache的顶级开源项目Tomcat了。
并且各大厂商只要有技术能力和资金的都会对这个开源项目进行定制，完成个性化的需求，例如：安全补丁，稳定性，内部分析等等。
作为一个后端开发来讲，如果不理解Tomcat的运行原理，就无法正确的配合物理资源进行开发。

## 一些反思：
软件工程最终的产品就是这个经过工程管理最后得到的软件，还有这些软件的升级调整等业务。所以对于开源代码的学习是非常重要的。
只有看过的代码足够多，并且认真分析这些代码的组成过程，才能让自己的知识面没有盲点。才能支持自己的业务向上发展。

## Tomcat的编译和测试：

## Tomcat的功能描述：
Tomcat是一种web容器，用来接收http请求，并将请求得到的结果返回。既然要满足这个需求，一般的思路为：
要有一个网络连接通信维护者和一个请求处理者。通信维护者能够监听到请求并返回数据；请求处理者能够将请求进行分解处理，得到请求应该返回的数据。
Tomcat的设计核心就是这样的，由两大组件组成：Connector和Container。Connector负责的是底层的网络通信的实现，而Container负责的是上层servlet业务的实现

## Tomcat的启动逻辑：
只有知道了Tomcat启动过程，才能对Connector和Container是怎么初始化的了然于胸，知道了Connector和Container的初始化才能准确的把握其结构。

### 从启动脚本开始：
分析从当前tomcat的bin目录下的执行脚本startup.bat开始，最关键的步骤为：
```shell
set "EXECUTABLE=%CATALINA_HOME%\bin\catalina.bat"
call "%EXECUTABLE%" start %CMD_LINE_ARGS%
```
总体上：tomcat的startup.sh脚本主要用来判断环境，找到catalina.sh脚本源路径，将启动命令参数传递给catalina.bat执行。
继续进入catalina.bat查看关键代码：
```shell
if "%CLASSPATH%" == "" goto emptyClasspath
set "CLASSPATH=%CLASSPATH%;"
:emptyClasspath
set "CLASSPATH=%CLASSPATH%%CATALINA_HOME%\bin\bootstrap.jar"

if not "%CATALINA_TMPDIR%" == "" goto gotTmpdir
set "CATALINA_TMPDIR=%CATALINA_BASE%\temp"
:gotTmpdir

rem Add tomcat-juli.jar to classpath
rem tomcat-juli.jar can be over-ridden per instance
if not exist "%CATALINA_BASE%\bin\tomcat-juli.jar" goto juliClasspathHome
set "CLASSPATH=%CLASSPATH%;%CATALINA_BASE%\bin\tomcat-juli.jar"
goto juliClasspathDone
:juliClasspathHome
set "CLASSPATH=%CLASSPATH%;%CATALINA_HOME%\bin\tomcat-juli.jar"
:juliClasspathDone
```
这个代码将当前bin路径下的jar包添加到classpath中，供后续程序运行时查找对应的库。然后看看，具体从哪个库开始执行：
```shell
echo Using CATALINA_BASE:   "%CATALINA_BASE%"
echo Using CATALINA_HOME:   "%CATALINA_HOME%"
echo Using CATALINA_TMPDIR: "%CATALINA_TMPDIR%"
if ""%1"" == ""debug"" goto use_jdk
echo Using JRE_HOME:        "%JRE_HOME%"
goto java_dir_displayed
:use_jdk
echo Using JAVA_HOME:       "%JAVA_HOME%"
:java_dir_displayed
echo Using CLASSPATH:       "%CLASSPATH%"

set _EXECJAVA=%_RUNJAVA%
set MAINCLASS=org.apache.catalina.startup.Bootstrap
set ACTION=start
set SECURITY_POLICY_FILE=
set DEBUG_OPTS=
set JPDA=
```
通过环境变量设置了catalina.bat执行的参数，然后指定入口的类。然后开始执行从startup.bat中传入的start启动命令脚本：
```shell
:doStart
shift
if "%TITLE%" == "" set TITLE=Tomcat
set _EXECJAVA=start "%TITLE%" %_RUNJAVA%
if not ""%1"" == ""-security"" goto execCmd
shift
echo Using Security Manager
set "SECURITY_POLICY_FILE=%CATALINA_BASE%\conf\catalina.policy"
goto execCmd
```
这儿又指定了启动的配置文件。
知道了启动的jar包位置，jar包中的入口class名称，还有对应的配置文件，那么就可以进入：
apache-tomcat-9.0.0.M22-src\java\org\apache\catalina\startup\Bootstrap.java
这个代码文件进行查看了。

### Tomcat的启动入口代码：
根据上述脚本，我们可以将tomcat的启动入口定位，并且得到入口调用的具体参数：
org.apache.catalina.startup.Boostrap start
进入tomcat代码，查看对应包下的内容为：
```java
/**
* Main method and entry point when starting Tomcat via the provided
* scripts.
*
* @param args Command line arguments to be processed
*/
public static void main(String args[]) {

    if (daemon == null) {
        // Don't set daemon until init() has completed
        Bootstrap bootstrap = new Bootstrap();
        try {
            bootstrap.init();
        } catch (Throwable t) {
            handleThrowable(t);
            t.printStackTrace();
            return;
        }
        daemon = bootstrap;
    } else {
        // When running as a service the call to stop will be on a new
        // thread so make sure the correct class loader is used to prevent
        // a range of class not found exceptions.
        Thread.currentThread().setContextClassLoader(daemon.catalinaLoader);
    }

    try {
        String command = "start";
        if (args.length > 0) {
            command = args[args.length - 1];
        }

        if (command.equals("startd")) {
            args[args.length - 1] = "start";
            daemon.load(args);
            daemon.start();
        } else if (command.equals("stopd")) {
            args[args.length - 1] = "stop";
            daemon.stop();
        } else if (command.equals("start")) {
            daemon.setAwait(true);
            daemon.load(args);
            daemon.start();
        } else if (command.equals("stop")) {
            daemon.stopServer(args);
        } else if (command.equals("configtest")) {
            daemon.load(args);
            if (null==daemon.getServer()) {
                System.exit(1);
            }
            System.exit(0);
        } else {
            log.warn("Bootstrap: command \"" + command + "\" does not exist.");
        }
    } catch (Throwable t) {
        // Unwrap the Exception for clearer error reporting
        if (t instanceof InvocationTargetException &&
                t.getCause() != null) {
            t = t.getCause();
        }
        handleThrowable(t);
        t.printStackTrace();
        System.exit(1);
    }
}
```
也可以用jd-gui-windows-1.4.0这个工具来打开bin目录下的Bootstrap.jar文件来查看是否对应的就是这个代码。

通过代码，这个main方法主要做了4件事：
> - 实例化Boostrap对象，并调用其init方法；
> - 调用setAwait方法；
> - 调用load方法；
> - 调用start方法；

接下来，我们仔细的查看这个过程。

#### 实例化Boostrap对象：
在早期的tomcat中，boostrap的实例创建中，是没有初始化操作的，是放在init调用的时候的进行的。
但是在最新的tomcat9中，使用了类的静态代码块来完成这个初始化操作。这样对于一些基本环境的设置都在类的构造之前完成了。
具体的静态初始化代码块内容为：
```java
static {
    // Will always be non-null
    String userDir = System.getProperty("user.dir");

    // Home first
    String home = System.getProperty(Globals.CATALINA_HOME_PROP);
    File homeFile = null;

    if (home != null) {
        File f = new File(home);
        try {
            homeFile = f.getCanonicalFile();
        } catch (IOException ioe) {
            homeFile = f.getAbsoluteFile();
        }
    }

    if (homeFile == null) {
        // First fall-back. See if current directory is a bin directory
        // in a normal Tomcat install
        File bootstrapJar = new File(userDir, "bootstrap.jar");

        if (bootstrapJar.exists()) {
            File f = new File(userDir, "..");
            try {
                homeFile = f.getCanonicalFile();
            } catch (IOException ioe) {
                homeFile = f.getAbsoluteFile();
            }
        }
    }

    if (homeFile == null) {
        // Second fall-back. Use current directory
        File f = new File(userDir);
        try {
            homeFile = f.getCanonicalFile();
        } catch (IOException ioe) {
            homeFile = f.getAbsoluteFile();
        }
    }

    catalinaHomeFile = homeFile;
    System.setProperty(
            Globals.CATALINA_HOME_PROP, catalinaHomeFile.getPath());

    // Then base
    String base = System.getProperty(Globals.CATALINA_BASE_PROP);
    if (base == null) {
        catalinaBaseFile = catalinaHomeFile;
    } else {
        File baseFile = new File(base);
        try {
            baseFile = baseFile.getCanonicalFile();
        } catch (IOException ioe) {
            baseFile = baseFile.getAbsoluteFile();
        }
        catalinaBaseFile = baseFile;
    }
    System.setProperty(
            Globals.CATALINA_BASE_PROP, catalinaBaseFile.getPath());
}
```
上述代码块的主要作用就是检查当前路径下的配置文件是否存在，然后读入配置文件，设置系统的环境变量。然后调用默认的构造函数来完成类的初始化操作。

关于静态代码块，一些基本的概念为：
> - 它是随着类的加载而执行，只执行一次，并优先于主函数。具体说，静态代码块是由类调用的。类调用时，先执行静态代码块，然后才执行主函数的。
> - 静态代码块其实就是给类初始化的，而构造代码块是给对象初始化的。
> - 静态代码块中的变量是局部变量，与普通函数中的局部变量性质没有区别。
> - 一个类中可以有多个静态代码块

#### Boostrap调用init初始化方法：
创建一个Boostrap对象之后，然后是调用init方法来进行初始化设置，具体内容为：
```java
/**
* Initialize daemon.
* @throws Exception Fatal initialization error
*/
public void init() throws Exception {

    initClassLoaders();

    Thread.currentThread().setContextClassLoader(catalinaLoader);

    SecurityClassLoad.securityClassLoad(catalinaLoader);

    // Load our startup class and call its process() method
    if (log.isDebugEnabled())
        log.debug("Loading startup class");
    Class<?> startupClass =
        catalinaLoader.loadClass
        ("org.apache.catalina.startup.Catalina");
    Object startupInstance = startupClass.newInstance();

    // Set the shared extensions class loader
    if (log.isDebugEnabled())
        log.debug("Setting startup class properties");
    String methodName = "setParentClassLoader";
    Class<?> paramTypes[] = new Class[1];
    paramTypes[0] = Class.forName("java.lang.ClassLoader");
    Object paramValues[] = new Object[1];
    paramValues[0] = sharedLoader;
    Method method =
        startupInstance.getClass().getMethod(methodName, paramTypes);
    method.invoke(startupInstance, paramValues);

    catalinaDaemon = startupInstance;
}
```
这是一个统一的调用入口，包含很多其他的初始化操作，主要有：
> - 初始化类加载器:
```java
initClassLoaders();
```
> - 设置当前线程的类加载器：
```java
Thread.currentThread().setContextClassLoader(catalinaLoader);
```
> - 然后有一个奇怪名字的类加载器：SecurityClassLoad.securityClassLoad：
```java
SecurityClassLoad.securityClassLoad(catalinaLoader);
```
> - 用自己的类加载器加载这个类：org.apache.catalina.startup.Catalina：
```java
 // Load our startup class and call its process() method
if (log.isDebugEnabled())
    log.debug("Loading startup class");
Class<?> startupClass =
    catalinaLoader.loadClass
    ("org.apache.catalina.startup.Catalina");
Object startupInstance = startupClass.newInstance();
```
> - 然后获取参数，用这些参数调用刚刚加载器获取的类的实例的方法：
```java
// Set the shared extensions class loader
if (log.isDebugEnabled())
    log.debug("Setting startup class properties");
String methodName = "setParentClassLoader";
Class<?> paramTypes[] = new Class[1];
paramTypes[0] = Class.forName("java.lang.ClassLoader");
Object paramValues[] = new Object[1];
paramValues[0] = sharedLoader;
Method method =
    startupInstance.getClass().getMethod(methodName, paramTypes);
method.invoke(startupInstance, paramValues);
```
> - 最后将这个类返回：
```java
catalinaDaemon = startupInstance;
```

其中比较关键的内容有：
1 初始化一个类加载器；
2 使用了自定义的类加载器来完成整个servlet容器的类加载；
3 SecurityClassLoad的使用。
下面我们来逐一过一遍。

##### 初始化类加载器：
```java
private void initClassLoaders() {
    try {
        commonLoader = createClassLoader("common", null);
        if( commonLoader == null ) {
            // no config file, default to this loader - we might be in a 'single' env.
            commonLoader=this.getClass().getClassLoader();
        }
        catalinaLoader = createClassLoader("server", commonLoader);
        sharedLoader = createClassLoader("shared", commonLoader);
    } catch (Throwable t) {
        handleThrowable(t);
        log.error("Class loader creation threw exception", t);
        System.exit(1);
    }
}
```
所谓的类加载器，他的主要作用是：
新建一个Java对象的时候，JVM要将这个对象对应的字节码加载到内存中，这个字节码的原始信息存放在编译生成的.class文件中，类加载器需要将这些存储在硬盘（或者网络）的.class文件经过一些处理之后变成字节码在加载到内存中。
要创建用户自己的类加载器，只需要扩展java.lang.ClassLoader类，然后覆盖它的findClass(String name)方法即可，该方法根据参数指定类的名字，返回对应的Class对象的引用。

这儿使用了createClassLoader函数来简化上述操作：
```java
private ClassLoader createClassLoader(String name, ClassLoader parent)
    throws Exception {

    String value = CatalinaProperties.getProperty(name + ".loader");
    if ((value == null) || (value.equals("")))
        return parent;

    value = replace(value);

    List<Repository> repositories = new ArrayList<>();

    String[] repositoryPaths = getPaths(value);

    for (String repository : repositoryPaths) {
        // Check for a JAR URL repository
        try {
            @SuppressWarnings("unused")
            URL url = new URL(repository);
            repositories.add(
                    new Repository(repository, RepositoryType.URL));
            continue;
        } catch (MalformedURLException e) {
            // Ignore
        }

        // Local repository
        if (repository.endsWith("*.jar")) {
            repository = repository.substring
                (0, repository.length() - "*.jar".length());
            repositories.add(
                    new Repository(repository, RepositoryType.GLOB));
        } else if (repository.endsWith(".jar")) {
            repositories.add(
                    new Repository(repository, RepositoryType.JAR));
        } else {
            repositories.add(
                    new Repository(repository, RepositoryType.DIR));
        }
    }

    return ClassLoaderFactory.createClassLoader(repositories, parent);
}
```
可以看到是在读取配置文件，然后交给一个工厂来完成类加载器的生成。

具体方式为：
> - 1. 从CatalinaProperties中获取属性值：
进入apache-tomcat-9.0.0.M22-src\java\org\apache\catalina\startup\CatalinaProperties.java
在静态代码段中，对配置文件进行了设置：
```java
static {
        loadProperties();
}

/**
* Load properties.
*/
private static void loadProperties() {

    InputStream is = null;
    String fileName = "catalina.properties";

    try {
        String configUrl = System.getProperty("catalina.config");
        if (configUrl != null) {
            if (configUrl.indexOf('/') == -1) {
                // No '/'. Must be a file name rather than a URL
                fileName = configUrl;
            } else {
                is = (new URL(configUrl)).openStream();
            }
        }
    } catch (Throwable t) {
        handleThrowable(t);
    }

    if (is == null) {
        try {
            File home = new File(Bootstrap.getCatalinaBase());
            File conf = new File(home, "conf");
            File propsFile = new File(conf, fileName);
            is = new FileInputStream(propsFile);
        } catch (Throwable t) {
            handleThrowable(t);
        }
    }

    if (is == null) {
        try {
            is = CatalinaProperties.class.getResourceAsStream
                ("/org/apache/catalina/startup/catalina.properties");
        } catch (Throwable t) {
            handleThrowable(t);
        }
    }

    if (is != null) {
        try {
            properties = new Properties();
            properties.load(is);
        } catch (Throwable t) {
            handleThrowable(t);
            log.warn(t);
        } finally {
            try {
                is.close();
            } catch (IOException ioe) {
                log.warn("Could not close catalina properties file", ioe);
            }
        }
    }

    if ((is == null)) {
        // Do something
        log.warn("Failed to load catalina properties file");
        // That's fine - we have reasonable defaults.
        properties = new Properties();
    }

    // Register the properties as system properties
    Enumeration<?> enumeration = properties.propertyNames();
    while (enumeration.hasMoreElements()) {
        String name = (String) enumeration.nextElement();
        String value = properties.getProperty(name);
        if (value != null) {
            System.setProperty(name, value);
        }
    }
}
```
读取当前包下的catalina.properties这个文件，然后获取common.loader这个属性的值：
```xml
common.loader="${catalina.base}/lib","${catalina.base}/lib/*.jar","${catalina.home}/lib","${catalina.home}/lib/*.jar"
```
> - 2. 将这个这个属性的值做一些处理（因为有分好分割的多个，最终转换为一个list），得到一个Repository的列表，然后交给类加载器工厂处理，简化后的流程代码为：
```java
List<Repository> repositories = new ArrayList<>();
repositories.add(new Repository(repository, RepositoryType.URL));
ClassLoaderFactory.createClassLoader(repositories, parent);
```
其中，Repository这个类型是在ClassLoaderFactory内部定义的子类，进入apache-tomcat-9.0.0.M22-src\java\org\apache\catalina\startup\ClassLoaderFactory.java，查看代码为：
```java
public static class Repository {
    private final String location;
    private final RepositoryType type;

    public Repository(String location, RepositoryType type) {
        this.location = location;
        this.type = type;
    }

    public String getLocation() {
        return location;
    }

    public RepositoryType getType() {
        return type;
    }
}

/**
* Create and return a new class loader, based on the configuration
* defaults and the specified directory paths:
*
* @param repositories List of class directories, jar files, jar directories
*                     or URLS that should be added to the repositories of
*                     the class loader.
* @param parent Parent class loader for the new class loader, or
*  <code>null</code> for the system class loader.
* @return the new class loader
*
* @exception Exception if an error occurs constructing the class loader
*/
public static ClassLoader createClassLoader(List<Repository> repositories,
                                            final ClassLoader parent)
    throws Exception {

    if (log.isDebugEnabled())
        log.debug("Creating new class loader");

    // Construct the "class path" for this class loader
    Set<URL> set = new LinkedHashSet<>();

    if (repositories != null) {
        for (Repository repository : repositories)  {
            if (repository.getType() == RepositoryType.URL) {
                URL url = buildClassLoaderUrl(repository.getLocation());
                if (log.isDebugEnabled())
                    log.debug("  Including URL " + url);
                set.add(url);
            } else if (repository.getType() == RepositoryType.DIR) {
                File directory = new File(repository.getLocation());
                directory = directory.getCanonicalFile();
                if (!validateFile(directory, RepositoryType.DIR)) {
                    continue;
                }
                URL url = buildClassLoaderUrl(directory);
                if (log.isDebugEnabled())
                    log.debug("  Including directory " + url);
                set.add(url);
            } else if (repository.getType() == RepositoryType.JAR) {
                File file=new File(repository.getLocation());
                file = file.getCanonicalFile();
                if (!validateFile(file, RepositoryType.JAR)) {
                    continue;
                }
                URL url = buildClassLoaderUrl(file);
                if (log.isDebugEnabled())
                    log.debug("  Including jar file " + url);
                set.add(url);
            } else if (repository.getType() == RepositoryType.GLOB) {
                File directory=new File(repository.getLocation());
                directory = directory.getCanonicalFile();
                if (!validateFile(directory, RepositoryType.GLOB)) {
                    continue;
                }
                if (log.isDebugEnabled())
                    log.debug("  Including directory glob "
                        + directory.getAbsolutePath());
                String filenames[] = directory.list();
                if (filenames == null) {
                    continue;
                }
                for (int j = 0; j < filenames.length; j++) {
                    String filename = filenames[j].toLowerCase(Locale.ENGLISH);
                    if (!filename.endsWith(".jar"))
                        continue;
                    File file = new File(directory, filenames[j]);
                    file = file.getCanonicalFile();
                    if (!validateFile(file, RepositoryType.JAR)) {
                        continue;
                    }
                    if (log.isDebugEnabled())
                        log.debug("    Including glob jar file "
                            + file.getAbsolutePath());
                    URL url = buildClassLoaderUrl(file);
                    set.add(url);
                }
            }
        }
    }

    // Construct the class loader itself
    final URL[] array = set.toArray(new URL[set.size()]);
    if (log.isDebugEnabled())
        for (int i = 0; i < array.length; i++) {
            log.debug("  location " + i + " is " + array[i]);
        }

    return AccessController.doPrivileged(
        new PrivilegedAction<URLClassLoader>() {
            @Override
            public URLClassLoader run() {
                if (parent == null)
                    return new URLClassLoader(array);
                else
                    return new URLClassLoader(array, parent);
            }
        });
}
```
主要的流程就是，对当前传入的参数列表遍历，然后根据每一个参数的路径和类型，通过不同的方式进行类加载器的构建，主要的方式有：
```java
public enum RepositoryType {
    DIR,
    GLOB,
    JAR,
    URL
}
```
也就是Repository中的属性的类型，总体上，支持从：路径，全局设置，jar包和URL创建类加载器。


参考：
[Tomcat源码解读：ClassLoader的设计](http://www.cnblogs.com/f1194361820/p/4186232.html)








