package com.iscas.datasong.service.thriftService.requestorServiceImpl;

import com.iscas.datasong.service.thriftService.*;
import com.iscas.datasong.web.DataServiceController;
import org.apache.thrift.TException;
import org.apache.thrift.TProcessor;
import org.apache.thrift.server.TServer;
import org.apache.thrift.server.TSimpleServer;
import org.apache.thrift.transport.TServerSocket;
import org.apache.thrift.transport.TTransportException;

import java.io.UnsupportedEncodingException;

/**
 * Created by home on 2017/3/7.
 */
public class requestorServiceImpl implements requestorService.Iface {

    /** 7位ASCII字符，也叫作ISO646-US、Unicode字符集的基本拉丁块 */
    public static final String US_ASCII = "US-ASCII";

    /** ISO 拉丁字母表 No.1，也叫作 ISO-LATIN-1 */
    public static final String ISO_8859_1 = "ISO-8859-1";

    /** 8 位 UCS 转换格式 */
    public static final String UTF_8 = "UTF-8";

    /** 16 位 UCS 转换格式，Big Endian（最低地址存放高位字节）字节顺序 */
    public static final String UTF_16BE = "UTF-16BE";

    /** 16 位 UCS 转换格式，Little-endian（最高地址存放低位字节）字节顺序 */
    public static final String UTF_16LE = "UTF-16LE";

    /** 16 位 UCS 转换格式，字节顺序由可选的字节顺序标记来标识 */
    public static final String UTF_16 = "UTF-16";

    /** 中文超大字符集 */
    public static final String GBK = "GBK";

    /**
     * 字符串编码转换的实现方法
     * @param str  待转换编码的字符串
     * @param newCharset 目标编码
     * @return
     * @throws UnsupportedEncodingException
     */
    public String changeCharset(String str, String newCharset)
            throws UnsupportedEncodingException {
        if (str != null) {
            //用默认字符编码解码字符串。
            byte[] bs = str.getBytes();
            //用新的字符编码生成字符串
            return new String(bs, newCharset);
        }
        return null;
    }
    /**
     * 字符串编码转换的实现方法
     * @param str  待转换编码的字符串
     * @param oldCharset 原编码
     * @param newCharset 目标编码
     * @return
     * @throws UnsupportedEncodingException
     */
    public String changeCharset(String str, String oldCharset, String newCharset)
            throws UnsupportedEncodingException {
        if (str != null) {
            //用旧的字符编码解码字符串。解码可能会出现异常。
            byte[] bs = str.getBytes(oldCharset);
            //用新的字符编码生成字符串
            return new String(bs, newCharset);
        }
        return null;
    }

    /////////////////////////////////////////////////////////////////////////////////////

    @Override
    public GetDataResponse getQueryResponseData(queryDataRequest thing) throws TException {
        // 准备参数：
        GetDataResponse returnBean = new GetDataResponse();

        // 调用内部对象
        DataServiceController controller = new DataServiceController();

        // 确定调用参数：
        String dbName = thing.getDbName();
        String tableName = thing.getTableName();
        String id = thing.getId();

        // 字符集转换：UTF-8编码
        try{
            System.out.println("GBK,UTF_8:"+this.changeCharset(dbName,GBK,UTF_8));
            System.out.println("UTF_16,UTF_8:"+this.changeCharset(dbName,UTF_16,UTF_8));
            System.out.println("UTF_16LE,UTF_8:"+this.changeCharset(dbName,UTF_16LE,UTF_8));
            System.out.println("UTF_16BE,UTF_8:"+this.changeCharset(dbName,UTF_16BE,UTF_8));
            System.out.println("US_ASCII,UTF_8:"+this.changeCharset(dbName,US_ASCII,UTF_8));
            System.out.println("US_ASCII,GBK:"+this.changeCharset(dbName,US_ASCII,GBK));
            System.out.println("-----------------------------------");
            System.out.println(this.changeCharset(dbName,UTF_8));
            System.out.println(this.changeCharset(dbName,UTF_16));
            System.out.println(this.changeCharset(dbName,UTF_16LE));
            System.out.println(this.changeCharset(dbName,UTF_16BE));
            System.out.println(this.changeCharset(dbName,GBK));
        }catch (UnsupportedEncodingException e){
            e.printStackTrace();
        }



        // 获取调用结果：
        //com.iscas.datasong.lib.response.data.GetDataResponse result = controller.getData(dbName,tableName,id);

        /*
        returnBean.setStatus(result.getStatus());
        returnBean.setDBName(result.getDbName());
        returnBean.setTableName(result.getTableName());
        returnBean.setInfo(result.getInfo());
        returnBean.setDataId(result.getDataId());
        returnBean.setVersion(result.getVersion());
        returnBean.setSource(result.getSource());
        */


        // 转换参数：
        try{
            returnBean.setStatus(404);
            returnBean.setDBName("testDBName");
            returnBean.setTableName("testTableName");
            //returnBean.setInfo(changeCharset("testInfo包含中文测试",GBK,UTF_8));
            returnBean.setInfo(changeCharset("testInfo包含中文测试",UTF_8,UTF_8));
            //returnBean.setInfo(changeCharset("testInfo包含中文测试",UTF_16BE,UTF_8));
            //returnBean.setInfo(changeCharset("testInfo包含中文测试",UTF_16LE,UTF_8));
            returnBean.setDataId("testID");
            returnBean.setVersion(1234567);
            returnBean.setSource("testSource");
        }catch (UnsupportedEncodingException e){
            e.printStackTrace();
        }


        // 返回获取结果：
        return returnBean;
    }

    @Override
    public GetDataResponse getTermBase(String tableName, String _id) throws TException {
        String querySqlStr = "SELECT * FROM";

        QueryBean bean;
        bean.setta();

        return null;
    }

    @Override
    public SearchDataResponse getAllBase(String tableName, int start, int size, String sortBy, boolean isAsc) throws TException {
        return null;
    }

    @Override
    public SearchDataResponse getByTermQueryBaseString(String tableName, String field, String value, int start, int size, String sortBy, boolean isAsc) throws TException {
        return null;
    }

    @Override
    public SearchDataResponse getByTermQueryBaseInt(String tableName, String field, int value, int start, int size, String sortBy, boolean isAsc) throws TException {
        return null;
    }

    @Override
    public SearchDataResponse getByMutiTermQueryBase(String tableName, String field1, String value1, String field2, String value2, int start, int size, String sortBy, boolean isAsc) throws TException {
        return null;
    }

    @Override
    public SearchDataResponse getByFulltextQueryBase(String tableName, String field, String value, int start, int size, String sortBy, boolean isAsc) throws TException {
        return null;
    }

    @Override
    public DeleteDataResponse deleteDataBase(String tableName, String _id) throws TException {
        return null;
    }

    @Override
    public PutDataResponse insertDataBase(String tableName, String jsonData) throws TException {
        return null;
    }


    // 测试入口：
    public static void main(String[] args){
        try {
            TServerSocket serverTransport = new TServerSocket(7911);
            //Factory proFactory = new TBinaryProtocol.Factory();
            TProcessor processor = new requestorService.Processor(new requestorServiceImpl());
            TServer server = new TSimpleServer(new TServer.Args(serverTransport).processor(processor));
            //TServer server = new TThreadPoolServer(processor, serverTransport,  proFactory);
            System.out.println("Start server on port 7911...");
            // 这儿会挂起来，导致后续进程失败，需要使用线程池来单开
            server.serve();
        } catch (TTransportException e) {
            e.printStackTrace();
        }
    }
}
