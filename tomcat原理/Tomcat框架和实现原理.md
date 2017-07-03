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
                - [设置当前线程的上下文类加载器：](#设置当前线程的上下文类加载器)
                - [使用这个加载器来加载自己的类：](#使用这个加载器来加载自己的类)
                - [最后将这个实例作为catalinaDaemon：](#最后将这个实例作为catalinadaemon)
            - [Boostrap调用setAwait方法：](#boostrap调用setawait方法)
            - [Boostrap调用load方法实现服务器的初始化：](#boostrap调用load方法实现服务器的初始化)
                - [初始化操作：](#初始化操作)
                - [配置文件server.xml的读入：](#配置文件serverxml的读入)
                - [设置标准输出和错误输出为 SystemLogHandler 接管：](#设置标准输出和错误输出为-systemloghandler-接管)
                - [服务的启动：](#服务的启动)
                - [启动时间计算：](#启动时间计算)
            - [Tomcat启动过程梳理：](#tomcat启动过程梳理)
                - [脚本启动和关闭服务：](#脚本启动和关闭服务)
                - [配置文件解析和服务生成详解：](#配置文件解析和服务生成详解)
                - [实例化的server启动：](#实例化的server启动)
                - [Service.init()的调用：](#serviceinit的调用)
                - [完整的Server.init()调用流程：](#完整的serverinit调用流程)
            - [Boostrap调用start方法启动服务器：](#boostrap调用start方法启动服务器)

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

上述代码中，实现了三个自定义类加载器的初始化：
```java
commonLoader = createClassLoader("common", null);
catalinaLoader = createClassLoader("server", commonLoader);
sharedLoader = createClassLoader("shared", commonLoader);
```
通过参数来设置类加载器的名称和父加载器，并且使用了createClassLoader函数来简化类加载器的生成操作，我们进入createClassLoader看看是如何实现的：
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

createClassLoader的主要流程为：
> - 首先创建一个Set容器，用来存放要创建的类加载器的URL：
```java
Set<URL> set = new LinkedHashSet<>();
```
> - 然后对传入的列表遍历，根据当前输入参数的类型做参数检查，并且获取每一个变量的location属性，最后都是用buildClassLoaderUrl这个函数来生成一个URL对象，加入到容器中。例如：
```java
if (repository.getType() == RepositoryType.URL) {
    URL url = buildClassLoaderUrl(repository.getLocation());
    if (log.isDebugEnabled())
        log.debug("  Including URL " + url);
    set.add(url);
}
```
> - 最后用Set中的URL来构建出类加载器：
```java
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
```
这个是对java.security.AccessController的使用，也就是用了java的安全模式。
doPrivileged 方法能够使一段受信任代码获得更大的权限，甚至比调用它的应用程序还要多，可做到临时访问更多的资源。
例如，应用程序可能无法直接访问某些系统资源，但这样的应用程序必须得到这些资源才能够完成功能。针对这种情况，Java SDK 给域提供了 doPrivileged 方法，让程序突破当前域权限限制，临时扩大访问权限。下面内容会详细讲解一下安全相关的方法使用。

上述函数最后返回的时候，在doPrivileged中重载了run方法，其中就包含了构造类加载器的方法调用：URLClassLoader。
这儿有一个问题，在创建common这个类加载器的时候，parent为null，但是所有的java类加载器都是有父的，这儿也说明了当父加载器没有设置的时候，使用 new URLClassLoader(array) 来构建这个加载器，也就是说最终会以系统类加载器(AppClassLoader)作为父类加载器。
如果指定了父加载器，就会在URLClassLoader中设置其父加载器，完成构造。

从JDK源码上来看其实是URLClassLoader继承了ClassLoader，也就是说URLClassLoader把ClassLoader扩展了一下，所以可以理解成URLClassLoader功能要多点。ClassLoader只能加载classpath下面的类，而URLClassLoader可以加载任意路径下的类。他们的继承关系如下：
```java
public class URLClassLoader extends SecureClassLoader {}
public class SecureClassLoader extends ClassLoader {}
```

返回到initClassLoader中：
catalinaLoader = createClassLoader("server", commonLoader);
sharedLoader = createClassLoader("shared", commonLoader);
说明这两个自定义类加载器的父是commonLoader这个自定义类加载器，到此为止，自定义的类加载器构造就完毕了。


参考：
> - [Tomcat源码解读：ClassLoader的设计](http://www.cnblogs.com/f1194361820/p/4186232.html)
> - [java安全模型介绍](https://www.ibm.com/developerworks/cn/java/j-lo-javasecurity/)
> - [java与tomcat7类加载机制](http://blog.csdn.net/czmacd/article/details/54017027)


##### 设置当前线程的上下文类加载器：
完成了自定义类加载器的初始化之后，就可以使用这些类加载器来完成类的加载了。

类 java.lang.Thread中的方法 getContextClassLoader()和setContextClassLoader(ClassLoader cl)用来获取和设置线程的上下文类加载器。如果没有通过 setContextClassLoader(ClassLoader cl)方法进行设置的话，线程将继承其父线程的上下文类加载器。Java 应用运行的初始线程的上下文类加载器是系统类加载器(appClassLoader)。在线程中运行的代码可以通过此类加载器来加载类和资源。

也就是说，如果没有将自定义的类加载器明确的设置为线程的上下文类加载器，那么就会使用系统类加载器，这样做的话就无法对不同的servlet容器实例提供不同的运行环境了，所以必然会用自定义的类加载器来替换系统的：
```java
Thread.currentThread().setContextClassLoader(catalinaLoader);
```
设置的是catalinaLoader这个类加载器，是commonloader的子加载器。

##### 使用这个加载器来加载自己的类：
使用自定义的类加载器加载指定路径的类：
```java
if (log.isDebugEnabled())
    log.debug("Loading startup class");
    Class<?> startupClass = catalinaLoader.loadClass("org.apache.catalina.startup.Catalina");
```
接着，调用Catalina的setParentClassLoader方法：
```java
Object startupInstance = startupClass.newInstance();

// Set the shared extensions class loader
if (log.isDebugEnabled())
    log.debug("Setting startup class properties");
String methodName = "setParentClassLoader";
Class<?> paramTypes[] = new Class[1];
paramTypes[0] = Class.forName("java.lang.ClassLoader");
Object paramValues[] = new Object[1];
paramValues[0] = sharedLoader;
Method method = startupInstance.getClass().getMethod(methodName, paramTypes);
method.invoke(startupInstance, paramValues);
```

##### 最后将这个实例作为catalinaDaemon：
```java
catalinaDaemon = startupInstance;
```
这个catalinaDaemon后续用来实现对Catalina的函数调用，完成后续操作。


参考：
[ClassLoader，Thread.currentThread().setContextClassLoader，tomcat的ClassLoader](http://www.cnblogs.com/549294286/p/3714692.html)


#### Boostrap调用setAwait方法：
完成了类加载的初始化之后，获取到了当前Boostrap的实例，然后在start参数下，第一个调用的方式就是setAwait方法。
调用的方式也是通过类加载器返回的实例进行调用的：
```java
/**
* Set flag.
* @param await <code>true</code> if the daemon should block
* @throws Exception Reflection error
*/
public void setAwait(boolean await)
    throws Exception {

    Class<?> paramTypes[] = new Class[1];
    paramTypes[0] = Boolean.TYPE;
    Object paramValues[] = new Object[1];
    paramValues[0] = Boolean.valueOf(await);
    Method method =
        catalinaDaemon.getClass().getMethod("setAwait", paramTypes);
    method.invoke(catalinaDaemon, paramValues);

}
```
进入apache-tomcat-9.0.0.M22-src\java\org\apache\catalina\startup\Catalina.java，看一下setAwait方法实现：
```java
public void setAwait(boolean b) {
    await = b;
}
```
非常简单，就是设置了一个标志位，问题的关键是这个标志位是用来干什么的？

#### Boostrap调用load方法实现服务器的初始化：
接下来就是调用load方法来获取启动的参数：
```java
/**
* Load daemon.
*/
private void load(String[] arguments)
    throws Exception {

    // Call the load() method
    String methodName = "load";
    Object param[];
    Class<?> paramTypes[];
    if (arguments==null || arguments.length==0) {
        paramTypes = null;
        param = null;
    } else {
        paramTypes = new Class[1];
        paramTypes[0] = arguments.getClass();
        param = new Object[1];
        param[0] = arguments;
    }
    Method method =
        catalinaDaemon.getClass().getMethod(methodName, paramTypes);
    if (log.isDebugEnabled())
        log.debug("Calling startup class " + method);
    method.invoke(catalinaDaemon, param);
}
```
也是通过类加载器获取的实例来完成调用的，参数为使用catalina.bat启动时传入的参数。
然后看看实际被调用的load函数的实现：
```java
/*
* Load using arguments
*/
public void load(String args[]) {

    try {
        if (arguments(args)) {
            load();
        }
    } catch (Exception e) {
        e.printStackTrace(System.out);
    }
}
```
这个函数中使用arguments函数处理了传入的参数：
```java
/**
* Process the specified command line arguments.
*
* @param args Command line arguments to process
* @return <code>true</code> if we should continue processing
*/
protected boolean arguments(String args[]) {

    boolean isConfig = false;

    if (args.length < 1) {
        usage();
        return false;
    }

    for (int i = 0; i < args.length; i++) {
        if (isConfig) {
            configFile = args[i];
            isConfig = false;
        } else if (args[i].equals("-config")) {
            isConfig = true;
        } else if (args[i].equals("-nonaming")) {
            setUseNaming(false);
        } else if (args[i].equals("-help")) {
            usage();
            return false;
        } else if (args[i].equals("start")) {
            // NOOP
        } else if (args[i].equals("configtest")) {
            // NOOP
        } else if (args[i].equals("stop")) {
            // NOOP
        } else {
            usage();
            return false;
        }
    }

    return true;
}
```
这个函数将输入的参数进行检查，然后打印使用说明等，最终调用了无参数的load方法：
```java
/**
* Start a new server instance.
*/
public void load() {

    long t1 = System.nanoTime();

    initDirs();

    // Before digester - it may be needed
    initNaming();

    // Create and execute our Digester
    Digester digester = createStartDigester();

    InputSource inputSource = null;
    InputStream inputStream = null;
    File file = null;
    try {
        try {
            file = configFile();
            inputStream = new FileInputStream(file);
            inputSource = new InputSource(file.toURI().toURL().toString());
        } catch (Exception e) {
            if (log.isDebugEnabled()) {
                log.debug(sm.getString("catalina.configFail", file), e);
            }
        }
        if (inputStream == null) {
            try {
                inputStream = getClass().getClassLoader()
                    .getResourceAsStream(getConfigFile());
                inputSource = new InputSource
                    (getClass().getClassLoader()
                        .getResource(getConfigFile()).toString());
            } catch (Exception e) {
                if (log.isDebugEnabled()) {
                    log.debug(sm.getString("catalina.configFail",
                            getConfigFile()), e);
                }
            }
        }

        // This should be included in catalina.jar
        // Alternative: don't bother with xml, just create it manually.
        if (inputStream == null) {
            try {
                inputStream = getClass().getClassLoader()
                        .getResourceAsStream("server-embed.xml");
                inputSource = new InputSource
                (getClass().getClassLoader()
                        .getResource("server-embed.xml").toString());
            } catch (Exception e) {
                if (log.isDebugEnabled()) {
                    log.debug(sm.getString("catalina.configFail",
                            "server-embed.xml"), e);
                }
            }
        }


        if (inputStream == null || inputSource == null) {
            if  (file == null) {
                log.warn(sm.getString("catalina.configFail",
                        getConfigFile() + "] or [server-embed.xml]"));
            } else {
                log.warn(sm.getString("catalina.configFail",
                        file.getAbsolutePath()));
                if (file.exists() && !file.canRead()) {
                    log.warn("Permissions incorrect, read permission is not allowed on the file.");
                }
            }
            return;
        }

        try {
            inputSource.setByteStream(inputStream);
            digester.push(this);
            digester.parse(inputSource);
        } catch (SAXParseException spe) {
            log.warn("Catalina.start using " + getConfigFile() + ": " +
                    spe.getMessage());
            return;
        } catch (Exception e) {
            log.warn("Catalina.start using " + getConfigFile() + ": " , e);
            return;
        }
    } finally {
        if (inputStream != null) {
            try {
                inputStream.close();
            } catch (IOException e) {
                // Ignore
            }
        }
    }

    getServer().setCatalina(this);
    getServer().setCatalinaHome(Bootstrap.getCatalinaHomeFile());
    getServer().setCatalinaBase(Bootstrap.getCatalinaBaseFile());

    // Stream redirection
    initStreams();

    // Start the new server
    try {
        getServer().init();
    } catch (LifecycleException e) {
        if (Boolean.getBoolean("org.apache.catalina.startup.EXIT_ON_INIT_FAILURE")) {
            throw new java.lang.Error(e);
        } else {
            log.error("Catalina.start", e);
        }
    }

    long t2 = System.nanoTime();
    if(log.isInfoEnabled()) {
        log.info("Initialization processed in " + ((t2 - t1) / 1000000) + " ms");
    }
}
```
这段代码比较长，核心功能是启动了一个server，我们快接近tomcat的container实现了！下面我们来认真看一下这段代码的实现过程。

参考：
> - [Tomcat6源码解析--Bootstrap.java](http://blog.csdn.net/wzy26816812/article/details/40371809)

##### 初始化操作：
这个函数执行的开始，首先记录一下当前的时间点：
```java
long t1 = System.nanoTime();
```
然后开始初始化目录：
```java
initDirs();
```
具体实现为：
```java
protected void initDirs() {
    String temp = System.getProperty("java.io.tmpdir");
    if (temp == null || (!(new File(temp)).isDirectory())) {
        log.error(sm.getString("embedded.notmp", temp));
    }
}
```
通过 System.getProperty("java.io.tmpdir") 是获取操作系统的缓存临时目录，然后判断这个目录是否存在。
接着调用 initNaming() 检查是否需要开启命名服务支持：
```java
protected boolean useNaming = true;

protected void initNaming() {
    // Setting additional variables
    if (!useNaming) {
        log.info( "Catalina naming disabled");
        System.setProperty("catalina.useNaming", "false");
    } else {
        System.setProperty("catalina.useNaming", "true");
        String value = "org.apache.naming";
        String oldValue =
            System.getProperty(javax.naming.Context.URL_PKG_PREFIXES);
        if (oldValue != null) {
            value = value + ":" + oldValue;
        }
        System.setProperty(javax.naming.Context.URL_PKG_PREFIXES, value);
        if( log.isDebugEnabled() ) {
            log.debug("Setting naming prefix=" + value);
        }
        value = System.getProperty
            (javax.naming.Context.INITIAL_CONTEXT_FACTORY);
        if (value == null) {
            System.setProperty
                (javax.naming.Context.INITIAL_CONTEXT_FACTORY,
                    "org.apache.naming.java.javaURLContextFactory");
        } else {
            log.debug( "INITIAL_CONTEXT_FACTORY already set " + value );
        }
    }
}
```
默认是命名服务支持的开关是打开的，将系统属性添加 "catalina.useNaming" ，设置这个属性值为true。

这个命名服务是干什么用的？
该方法主要用于设置javax.naming的一些需要用于的属性值,如javax.naming.Context.INITIAL_CONTEXT_FACTORY及javax.naming.Context.URL_PKG_PREFIXES

例如：
在具体的解析中，类StandardContext中进行启动时，只需要判断System.getProperty("catalina.useNaming")即可判断出是否启动命名上下文。如果启用命名服务，则会自动将NamingContextListener注册到监听器中，余下的工作就是监听器去完成了。
也就是说这个命名服务支持是用来实现tomcat的JNDI支持的。

参考：
[在embed tomcat中使用jndi命名服务](https://www.iflym.com/index.php/code/201208080003.html)

##### 配置文件server.xml的读入：
我们在启动tomcat之前，往往会设置conf文件夹下的server.xml来定制tomcat服务器的行为，所以对于server.xml的获取非常重要。
创建一个digester来获取配置文件，并且解析：
```java
// Create and execute our Digester
Digester digester = createStartDigester();

InputSource inputSource = null;
InputStream inputStream = null;
File file = null;
try {
    try {
        file = configFile();
        inputStream = new FileInputStream(file);
        inputSource = new InputSource(file.toURI().toURL().toString());
    } catch (Exception e) {
        if (log.isDebugEnabled()) {
            log.debug(sm.getString("catalina.configFail", file), e);
        }
    }
    if (inputStream == null) {
        try {
            inputStream = getClass().getClassLoader()
                .getResourceAsStream(getConfigFile());
            inputSource = new InputSource
                (getClass().getClassLoader()
                    .getResource(getConfigFile()).toString());
        } catch (Exception e) {
            if (log.isDebugEnabled()) {
                log.debug(sm.getString("catalina.configFail",
                        getConfigFile()), e);
            }
        }
    }

    // This should be included in catalina.jar
    // Alternative: don't bother with xml, just create it manually.
    if (inputStream == null) {
        try {
            inputStream = getClass().getClassLoader()
                    .getResourceAsStream("server-embed.xml");
            inputSource = new InputSource
            (getClass().getClassLoader()
                    .getResource("server-embed.xml").toString());
        } catch (Exception e) {
            if (log.isDebugEnabled()) {
                log.debug(sm.getString("catalina.configFail",
                        "server-embed.xml"), e);
            }
        }
    }


    if (inputStream == null || inputSource == null) {
        if  (file == null) {
            log.warn(sm.getString("catalina.configFail",
                    getConfigFile() + "] or [server-embed.xml]"));
        } else {
            log.warn(sm.getString("catalina.configFail",
                    file.getAbsolutePath()));
            if (file.exists() && !file.canRead()) {
                log.warn("Permissions incorrect, read permission is not allowed on the file.");
            }
        }
        return;
    }

    try {
        inputSource.setByteStream(inputStream);
        digester.push(this);
        digester.parse(inputSource);
    } catch (SAXParseException spe) {
        log.warn("Catalina.start using " + getConfigFile() + ": " +
                spe.getMessage());
        return;
    } catch (Exception e) {
        log.warn("Catalina.start using " + getConfigFile() + ": " , e);
        return;
    }
} finally {
    if (inputStream != null) {
        try {
            inputStream.close();
        } catch (IOException e) {
            // Ignore
        }
    }
}

getServer().setCatalina(this);
getServer().setCatalinaHome(Bootstrap.getCatalinaHomeFile());
getServer().setCatalinaBase(Bootstrap.getCatalinaBaseFile());
```
总体上，均是TOMCAT服务器读取配置文件${catalina.home}/conf/server.xml的过程,这个过程也是整个服务器启动的最重要的一个过程，这个过程比较复杂，暂时不看了。

参考：
[tomcat解析(五)Digester(一)](http://blog.csdn.net/holly2k/article/details/5258829)
[tomcat解析(六)Digester(二)startElement](http://blog.csdn.net/holly2k/article/details/5258840)
[tomcat解析(七)Digester(四)characters,endElement.endDocument](http://blog.csdn.net/holly2k/article/details/5261156)
[tomcat解析(八)Catalina.createStartDigester](http://blog.csdn.net/holly2k/article/details/5258849)

##### 设置标准输出和错误输出为 SystemLogHandler 接管：
initStreams() 这个函数将System.out和System.error进行了重定向：
```java
protected void initStreams() {
    // Replace System.out and System.err with a custom PrintStream
    System.setOut(new SystemLogHandler(System.out));
    System.setErr(new SystemLogHandler(System.err));
}
```
这儿使用了System.setOut和setErr函数来做输出的重定向。重定向到自定义的SystemLogHandler类中做处理。

##### 服务的启动：
在完成了从server.xml配置文件中获取参数对server构造，并且重定向当前输出之后，就开始对这个server启动了：
```java
// Start the new server
try {
    getServer().init();
} catch (LifecycleException e) {
    if (Boolean.getBoolean("org.apache.catalina.startup.EXIT_ON_INIT_FAILURE")) {
        throw new java.lang.Error(e);
    } else {
        log.error("Catalina.start", e);
    }
}
```
这儿的getServer获取的是通过server.xml配置文件来动态生成的Server接口的一个实现，然后调用其init方法来启动。
可以看到这一步其实是很从配置文件实例化一个server密切相关的，只是中间因为需要接管标准输出多加入了一步。

##### 启动时间计算：
最后这一步就是在命令行中启动tomcat最常见的内容了，整个服务的启动，开销的时间：
```java
long t2 = System.nanoTime();
if(log.isInfoEnabled()) {
    log.info("Initialization processed in " + ((t2 - t1) / 1000000) + " ms");
}
```

至此，tomcat的服务启动过程就结束了。

#### Tomcat启动过程梳理：
上述整个流程从启动脚本到内部实现，总体上完成了对tomcat服务启动的描述，但是其中包含很多的细节还缺少，导致很多内容并没出现在启动分析中。
最重要的server的实例生成依赖于对xml的解析，使用的工具是：[commons-digester](https://commons.apache.org/proper/commons-digester/)。

##### 脚本启动和关闭服务：
我们在使用脚本的时候，可以在任意时候对当前的tomcat实例进行开启和关闭，如何保证操作的是同一个实例？
我们使用脚本启动和关闭tomcat的时候，实际上最终都是执行bootstrap的main方法，正因为daemon是static的，所以，我们start和stop的时候，实际上操作的是同一个bootstrap对象，才能对同一个tomcat的启动和关闭。 

##### 配置文件解析和服务生成详解：
再来回过头看看createStartDigester这个函数创建digester的具体实现：
```java
/**
* Create and configure the Digester we will be using for startup.
* @return the main digester to parse server.xml
*/
protected Digester createStartDigester() {
    long t1=System.currentTimeMillis();
    // Initialize the digester
    Digester digester = new Digester();
    digester.setValidating(false);
    digester.setRulesValidation(true);
    Map<Class<?>, List<String>> fakeAttributes = new HashMap<>();
    List<String> attrs = new ArrayList<>();
    attrs.add("className");
    fakeAttributes.put(Object.class, attrs);
    digester.setFakeAttributes(fakeAttributes);
    digester.setUseContextClassLoader(true);

    // Configure the actions we will be using
    digester.addObjectCreate("Server",
                                "org.apache.catalina.core.StandardServer",
                                "className");
    digester.addSetProperties("Server");
    digester.addSetNext("Server",
                        "setServer",
                        "org.apache.catalina.Server");

    digester.addObjectCreate("Server/GlobalNamingResources",
                                "org.apache.catalina.deploy.NamingResourcesImpl");
    digester.addSetProperties("Server/GlobalNamingResources");
    digester.addSetNext("Server/GlobalNamingResources",
                        "setGlobalNamingResources",
                        "org.apache.catalina.deploy.NamingResourcesImpl");

    digester.addObjectCreate("Server/Listener",
                                null, // MUST be specified in the element
                                "className");
    digester.addSetProperties("Server/Listener");
    digester.addSetNext("Server/Listener",
                        "addLifecycleListener",
                        "org.apache.catalina.LifecycleListener");

    digester.addObjectCreate("Server/Service",
                                "org.apache.catalina.core.StandardService",
                                "className");
    digester.addSetProperties("Server/Service");
    digester.addSetNext("Server/Service",
                        "addService",
                        "org.apache.catalina.Service");

    digester.addObjectCreate("Server/Service/Listener",
                                null, // MUST be specified in the element
                                "className");
    digester.addSetProperties("Server/Service/Listener");
    digester.addSetNext("Server/Service/Listener",
                        "addLifecycleListener",
                        "org.apache.catalina.LifecycleListener");

    //Executor
    digester.addObjectCreate("Server/Service/Executor",
                        "org.apache.catalina.core.StandardThreadExecutor",
                        "className");
    digester.addSetProperties("Server/Service/Executor");

    digester.addSetNext("Server/Service/Executor",
                        "addExecutor",
                        "org.apache.catalina.Executor");


    digester.addRule("Server/Service/Connector",
                        new ConnectorCreateRule());
    digester.addRule("Server/Service/Connector", new SetAllPropertiesRule(
            new String[]{"executor", "sslImplementationName", "protocol"}));
    digester.addSetNext("Server/Service/Connector",
                        "addConnector",
                        "org.apache.catalina.connector.Connector");

    digester.addObjectCreate("Server/Service/Connector/SSLHostConfig",
                                "org.apache.tomcat.util.net.SSLHostConfig");
    digester.addSetProperties("Server/Service/Connector/SSLHostConfig");
    digester.addSetNext("Server/Service/Connector/SSLHostConfig",
            "addSslHostConfig",
            "org.apache.tomcat.util.net.SSLHostConfig");

    digester.addRule("Server/Service/Connector/SSLHostConfig/Certificate",
                        new CertificateCreateRule());
    digester.addRule("Server/Service/Connector/SSLHostConfig/Certificate",
                        new SetAllPropertiesRule(new String[]{"type"}));
    digester.addSetNext("Server/Service/Connector/SSLHostConfig/Certificate",
                        "addCertificate",
                        "org.apache.tomcat.util.net.SSLHostConfigCertificate");

    digester.addObjectCreate("Server/Service/Connector/Listener",
                                null, // MUST be specified in the element
                                "className");
    digester.addSetProperties("Server/Service/Connector/Listener");
    digester.addSetNext("Server/Service/Connector/Listener",
                        "addLifecycleListener",
                        "org.apache.catalina.LifecycleListener");

    digester.addObjectCreate("Server/Service/Connector/UpgradeProtocol",
                                null, // MUST be specified in the element
                                "className");
    digester.addSetProperties("Server/Service/Connector/UpgradeProtocol");
    digester.addSetNext("Server/Service/Connector/UpgradeProtocol",
                        "addUpgradeProtocol",
                        "org.apache.coyote.UpgradeProtocol");

    // Add RuleSets for nested elements
    digester.addRuleSet(new NamingRuleSet("Server/GlobalNamingResources/"));
    digester.addRuleSet(new EngineRuleSet("Server/Service/"));
    digester.addRuleSet(new HostRuleSet("Server/Service/Engine/"));
    digester.addRuleSet(new ContextRuleSet("Server/Service/Engine/Host/"));
    addClusterRuleSet(digester, "Server/Service/Engine/Host/Cluster/");
    digester.addRuleSet(new NamingRuleSet("Server/Service/Engine/Host/Context/"));

    // When the 'engine' is found, set the parentClassLoader.
    digester.addRule("Server/Service/Engine",
                        new SetParentClassLoaderRule(parentClassLoader));
    addClusterRuleSet(digester, "Server/Service/Engine/Cluster/");

    long t2=System.currentTimeMillis();
    if (log.isDebugEnabled()) {
        log.debug("Digester for server.xml created " + ( t2-t1 ));
    }
    return digester;

}
```
其中add开头的方法就是在给 Digester 配置一些规则：
```java
//创建一个创建server对象的规则
digester.addObjectCreate("Server","org.apache.catalina.core.StandardServer","className");
//创建一个设置属性的规则，那么创建完了server对象之后就会设置配置文件后面的属性
digester.addSetProperties("Server");
//当server元素执行完了之后，执行的操作。。。这里其实是调用前面那个对象的方法
digester.addSetNext("Server","setServer","org.apache.catalina.Server");

digester.addObjectCreate("Server/Listener",null,"className");
digester.addSetProperties("Server/Listener");
digester.addSetNext("Server/Listener","addLifecycleListener","org.apache.catalina.LifecycleListener");

digester.addObjectCreate("Server/Service","org.apache.catalina.core.StandardService","className");
digester.addSetProperties("Server/Service");
digester.addSetNext("Server/Service","addService","org.apache.catalina.Service");

digester.addObjectCreate("Server/Service/Listener",null,"className");
digester.addSetProperties("Server/Service/Listener");
digester.addSetNext("Server/Service/Listener","addLifecycleListener","org.apache.catalina.LifecycleListener");
```
有了这些规则之后，在执行parse的过程中，就会根据这些规则来做一些操作。比如，解析到server.xml中的Server节点的时候:
```xml
<Server port="8005" shutdown="SHUTDOWN">
  ...
  <Service name="Catalina">
  ...
  ...
```
就会根据上面的规则，先创建一个org.apache.catalina.core.StandardService对象，然后调用 StandardServer 的 addService 方法将他们关联起来。其他的组件也是按类似的方式完成创建和关联的。

这样经过对xml文件的解析将会产生org.apache.catalina.core.StandardServer、org.apache.catalina.core.StandardService、org.apache.catalina.connector.Connector、org.apache.catalina.core.StandardEngine、org.apache.catalina.core.StandardHost、org.apache.catalina.core.StandardContext等等一系列对象，这些对象从前到后前一个包含后一个对象的引用。


参考：
[【Tomcat学习笔记】4-启动流程分析](https://fdx321.github.io/2017/05/18/%E3%80%90Tomcat%E5%AD%A6%E4%B9%A0%E7%AC%94%E8%AE%B0%E3%80%914-%E5%90%AF%E5%8A%A8%E6%B5%81%E7%A8%8B%E5%88%86%E6%9E%90/)
[Apache Commons Digester 一 （基础内容、核心API）](http://www.cnblogs.com/chenpi/p/6930730.html)
[Apache Commons Digester 二（规则模块绑定-RulesModule、异步解析-asyncParse、xml变量Substitutor、带参构造方法）](http://www.cnblogs.com/chenpi/p/6947441.html)
[Apache Commons Digester 三（规则注解）](http://www.cnblogs.com/chenpi/p/6959691.html)
[XML 解析（二） JDOM, DOM4J,Digester](http://goalietang.iteye.com/blog/2030255)
[Tomcat源码的catalina中利用Digester解析conf/server.xml](http://blog.csdn.net/wgw335363240/article/details/5869660)


##### 实例化的server启动：
既然通过配置文件找到了需要启动的server和service，那么回到 getServer().init() 这里看看是如何启动的。

解析完xml之后关闭文件流，接着设置StandardServer对象（该对象在上面解析xml时）的catalina的引用为当前对象。接下来将调用StandardServer对象的init方法。

也就是说，现在的Server是StandardServer，那么执行的init方法也只StandardServer中的init方法，但是这个类中没有这个方法，那么这个调用就会去找他的父类的init方法：
```java
public final class StandardServer extends LifecycleMBeanBase implements Server 
```
而LifecycleMBeanBase也没有这个方法，继续向上找其父类的init方法，也就是LifecycleBase的init方法：
```java
@Override
public final synchronized void init() throws LifecycleException {
    if (!state.equals(LifecycleState.NEW)) {
        invalidTransition(Lifecycle.BEFORE_INIT_EVENT);
    }

    try {
        setStateInternal(LifecycleState.INITIALIZING, null, false);
        initInternal();
        setStateInternal(LifecycleState.INITIALIZED, null, false);
    } catch (Throwable t) {
        handleSubClassException(t, "lifecycleBase.initFail", toString());
    }
}
```
调用initInternal方法，这儿需要注意，因为实际上还是从StandardServer进行调用的，所以这儿的initInternal方法实际上执行的是StandardServer中的initInternal方法：
```java
/**
* Invoke a pre-startup initialization. This is used to allow connectors
* to bind to restricted ports under Unix operating environments.
*/
@Override
protected void initInternal() throws LifecycleException {

    super.initInternal();

    // Register global String cache
    // Note although the cache is global, if there are multiple Servers
    // present in the JVM (may happen when embedding) then the same cache
    // will be registered under multiple names
    onameStringCache = register(new StringCache(), "type=StringCache");

    // Register the MBeanFactory
    MBeanFactory factory = new MBeanFactory();
    factory.setContainer(this);
    onameMBeanFactory = register(factory, "type=MBeanFactory");

    // Register the naming resources
    globalNamingResources.init();

    // Populate the extension validator with JARs from common and shared
    // class loaders
    if (getCatalina() != null) {
        ClassLoader cl = getCatalina().getParentClassLoader();
        // Walk the class loader hierarchy. Stop at the system class loader.
        // This will add the shared (if present) and common class loaders
        while (cl != null && cl != ClassLoader.getSystemClassLoader()) {
            if (cl instanceof URLClassLoader) {
                URL[] urls = ((URLClassLoader) cl).getURLs();
                for (URL url : urls) {
                    if (url.getProtocol().equals("file")) {
                        try {
                            File f = new File (url.toURI());
                            if (f.isFile() &&
                                    f.getName().endsWith(".jar")) {
                                ExtensionValidator.addSystemResource(f);
                            }
                        } catch (URISyntaxException e) {
                            // Ignore
                        } catch (IOException e) {
                            // Ignore
                        }
                    }
                }
            }
            cl = cl.getParent();
        }
    }
    // Initialize our defined Services
    for (int i = 0; i < services.length; i++) {
        services[i].init();
    }
}
```
在开始执行的时候，首先执行的是：
```java
super.initInternal();
```
也就是StandardServer的父类的initInternal方法，也就是LifecycleMBeanBase中的initInternal方法：
```java
/**
* Sub-classes wishing to perform additional initialization should override
* this method, ensuring that super.initInternal() is the first call in the
* overriding method.
*/
@Override
protected void initInternal() throws LifecycleException {

    // If oname is not null then registration has already happened via
    // preRegister().
    if (oname == null) {
        mserver = Registry.getRegistry(null, null).getMBeanServer();

        oname = register(this, getObjectNameKeyProperties());
    }
}
```

StandardServer.initInternal()方法中主要完成的功能有：
> - 调用父类LifecycleMBeanBase.initInternal方法进行注册MBean
> - 从低向上逐级验证tomcat类加载器
> - 使用循环逐个初始化service（在解析serverx.xml的时候已经实例化StandardService并调用StandardServer.addService()添加到StandardServer.services变量中）。在标准server.xml配置中只有一个service——StandardService，所以就是只调用StandardService.init()这一个service的方法了。

调用getServer().init()方法后，会进入 LifecycleBase#init，这个方法主要是设置生命周期以及触发相应的事件，之后会调用 StandardServer#init()，它首先会调用 LifecycleMBeanBase#init 把自己注册到MBeanServer中(JMX后面会具体说)，然后完成 StandardServer 自己初始化需要做的事情，最后在遍历数组，依次调用各个service的init方法。

参考：
[Tomcat启动部署](http://www.jianshu.com/p/150bb9bffab9)

##### Service.init()的调用：
继续上述Server.init()调用中，最后调用的是Service的init方法。
这儿的调用和上述Server.init()的调用是一样的，还是通过父类来完成了中转调用：
StandardService中也没有init方法，只能找其父类，也就是LifecycleMBeanBase的init方法，也没有，继续向上找其父类，也就是LifecycleBase的init方法，有这个方法，其中又调用了initInternal()方法；
在StandardService中调用这个initInternal()方法为：
```java
/**
* Invoke a pre-startup initialization. This is used to allow connectors
* to bind to restricted ports under Unix operating environments.
*/
@Override
protected void initInternal() throws LifecycleException {

    super.initInternal();

    if (engine != null) {
        engine.init();
    }

    // Initialize any Executors
    for (Executor executor : findExecutors()) {
        if (executor instanceof JmxEnabled) {
            ((JmxEnabled) executor).setDomain(getDomain());
        }
        executor.init();
    }

    // Initialize mapper listener
    mapperListener.init();

    // Initialize our defined Connectors
    synchronized (connectorsLock) {
        for (Connector connector : connectors) {
            connector.init();
        }
    }
}
```
又首先调用了LifecycleMBeanBase的initInternal()方法。

各个Service调用init方法，总体上完成的主要功能有：
> - 调用父类LifecycleMBeanBase.initInternal方法进行注册MBeang
> - 如果container（这里的container是StandardEngine）不是null，则调用container的init方法进行初始化
> - 如果有Executor则逐个初始化
> - 最后使用循环逐个在初始化Connector，这里connector有两个，分别是用来处理两种协议：http和ajp

在这儿，Connector第一次完整的出现了。


##### 完整的Server.init()调用流程：

整体业务流程代码：
```java
Digester digester = createStartDigester();

inputSource.setByteStream(inputStream);

digester.push(this);

digester.parse(inputSource);

getServer().setCatalina(this);

getServer().init();
```

Serivce.init调用流程图：

![tupian](tomcat_StandardService_init()调用.png)

参考：
> - [How Tomcat works — 三、tomcat启动（2）](http://www.cnblogs.com/sunshine-2015/p/5745868.html)


#### Boostrap调用start方法启动服务器：
完成上述load之后，接下来在Boostrap中调用的就是start方法了：








