package com.iscas.datasong.service.thriftService.requestorServiceImpl;

import com.iscas.datasong.domain.sql.model.metadata.FileMetadata;
import com.iscas.datasong.lib.common.*;
import com.iscas.datasong.service.fileService.FileServiceImpl;
import com.iscas.datasong.service.sqlExecuteService.SqlExecuteService;
import com.iscas.datasong.service.sqlservice.metadata.FileMetadataService;
import com.iscas.datasong.service.thriftService.*;
import com.iscas.datasong.service.thriftService.DataItem;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.thrift.TException;
import org.apache.thrift.TProcessor;
import org.apache.thrift.server.TServer;
import org.apache.thrift.server.TSimpleServer;
import org.apache.thrift.transport.TServerSocket;
import org.apache.thrift.transport.TTransportException;
import org.codehaus.jackson.map.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Created by wentao on 2017/3/7.
 */
@Service
public class requestorServiceImpl implements DataSongRequestService.Iface {

    private static SqlExecuteService sqlExecutor;

    public requestorServiceImpl(){
    }

    @Autowired
    private void setSqlExecutor(SqlExecuteService sqlExecutor){
        this.sqlExecutor = sqlExecutor;
    }

    private static void BeanPropertyCopy(Object from, Object to) {
        Field[] fromFields = from.getClass().getDeclaredFields();
        Field[] toFields = to.getClass().getDeclaredFields();

        Class toClass = to.getClass();

        Set<String> toFieldSet = Stream.of(toFields).map(Field::getName).collect(Collectors.toSet());

        for (Field fromField : fromFields) {
            // 判断来源属性中是否含有目标属性
            if (toFieldSet.contains(fromField.getName())) {
                try {
                    Field toField = toClass.getDeclaredField(fromField.getName());
                    toField.set(to, fromField.get(from));
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
    }

	
	// Bean to Bean 按照属性拷贝：用于替换BeanUtils.beanpropertiescopy函数，支持嵌套的Bean定义
    private static void BeanPropertyCopy2(Object from, Object to) {
        // 获取当前源和目标的属性列表：
        Field[] fromFields = from.getClass().getDeclaredFields();
        Field[] toFields = to.getClass().getDeclaredFields();
        // 获取目标中的属性名称集合：
        Set<String> toFieldNameSet = Stream.of(toFields).map(Field::getName).collect(Collectors.toSet());
        Set<String> toFieldNameSetLowCase = Stream.of(toFields).map(Field::getName).map(String::toLowerCase).collect(Collectors.toSet());
        // 创建一个目标类型的实例：
        Class toClass = to.getClass();
        // 开始遍历查询属性，进行拷贝：
        for (Field fromField : fromFields) {
            // 获取当前源属性的名称和类型：
            String currentFromFiledName = fromField.getName();
            String currentFromFiledType = fromField.getType().getTypeName();
            //thrift private member access，设置当前源属性为可访问：
            fromField.setAccessible(true);
            // 在目标属性的名称集合中搜索当前源属性的名称是否存在，需要注意的是这儿是否需要区分大小写！
            // 不区分大小写：
            if (toFieldNameSetLowCase.contains(currentFromFiledName.toLowerCase())) {
            // 区分大小写：
            //if (toFieldNameSet.contains(currentFromFiledName)) {
                // 如果源属性是简单类型，直接复制到目标属性中
                if(currentFromFiledType!="java.util.List"){
                    //
                    try {
                        Field toField = toClass.getDeclaredField(fromField.getName());
                        toField.set(to, fromField.get(from));
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
                // 如果源属性是嵌套类型，需要递归调用：
                else{
                    try{
                        // 获取当前源的类型：
                        ParameterizedType subFromClassType = (ParameterizedType)fromField.getGenericType();
                        Class<?> subFromClass = (Class<?>) subFromClassType.getActualTypeArguments()[0];
                        System.out.println(subFromClass);

                        // 获取当前目标的类型：
                        Field toField = toClass.getDeclaredField(fromField.getName());
                        ParameterizedType subToClassType = (ParameterizedType)toField.getGenericType();
                        Class<?> subToClass = (Class<?>) subToClassType.getActualTypeArguments()[0];
                        System.out.println(subToClass);

                        // 获取当前的新目标和源：
                        List<?> newFrom = (List<?>)fromField.get(from);
                        // 准备数据：
                        List<Object> resultList = new ArrayList<>();
                        // 遍历拷贝：
                        Iterator<?> fiter = newFrom.iterator();
                        for(;fiter.hasNext();){
                            Object fromObj  = fiter.next();
                            Object toObj = subToClass.getDeclaredConstructor().newInstance();
                            BeanPropertyCopy2(fromObj,toObj);
                            resultList.add(toObj);
                        }
                        // 赋值：
                        toField.set(to, resultList);
                    }catch (Exception e) {
                        e.printStackTrace();
                    }
                }

            }
        }
    }

    @Override
    public GetDataResponse getItemQueryResponseData(String dbname, String tableName, String _id) throws TException {
        return null;
    }

    @Override
    public SearchDataResponse getQueryResponseData(String dbname, String sqlstr) throws TException {
        SearchDataResponse result = new SearchDataResponse();
        try{
            com.iscas.datasong.lib.response.data.SearchDataResponse originalBeanResult =
                    (com.iscas.datasong.lib.response.data.SearchDataResponse)this.sqlExecutor.excuteSQL(dbname, sqlstr);
            // 然后将内部类型转换为接口定义类型，提交给thrift，然后返回给cpp客户端：
            BeanPropertyCopy2(originalBeanResult, result);

            /*
            BeanUtils.copyProperties(result,originalBeanResult);

            result.setDBName(originalBeanResult.getDBName());
            result.setTableName(originalBeanResult.getTableName());
            result.setInfo(originalBeanResult.getInfo());
            // DataItem数组遍历复制
            Iterator<com.iscas.datasong.lib.common.DataItem> iterator;// = originalBeanResult.getItems().iterator();
            List<DataItem> resultDataItemList = new ArrayList<>();
            for (iterator = originalBeanResult.getItems().iterator(); iterator.hasNext();) {
                //DataItem tmp = iterator.next();
                //resultDataItemList.add(tmp);
            }
            result.setItems(resultDataItemList);
            */

        }catch (Exception e){
            e.printStackTrace();
        }

        return result;
    }

    @Override
    public InsertDataResponse getInsertResponseData(String dbname, String sqlstr) throws TException {
        InsertDataResponse result = new InsertDataResponse();
        try{
            com.iscas.datasong.lib.response.data.InsertDataResponse originalBeanResult =
                    (com.iscas.datasong.lib.response.data.InsertDataResponse)this.sqlExecutor.excuteSQL(dbname, sqlstr);
            BeanPropertyCopy(originalBeanResult, result);
        }catch (DataSongException e){
            e.printStackTrace();
        }

        return result;
    }

    @Override
    public DeleteDataResponse getDeleteResponseData(String dbname, String sqlstr) throws TException {
        DeleteDataResponse result = new DeleteDataResponse();
        try{
            com.iscas.datasong.lib.response.data.DeleteDataResponse originalBeanResult =
                    (com.iscas.datasong.lib.response.data.DeleteDataResponse)this.sqlExecutor.excuteSQL(dbname, sqlstr);
            BeanPropertyCopy(originalBeanResult, result);
        }catch (DataSongException e){
            e.printStackTrace();
        }

        return result;
    }

    // TODO: 确定动态获取文件的位置参数
    @Override
    public ByteBuffer getBinaryStreamResponseData(String dbname, String tableName, String _id) throws TException {
        // 一次读取的文件块大小：一次读入1M数据
        int byteSize = 1024 * 1024;
        // 获取的文件块缓存：
        byte[] buffer = new byte[byteSize];
        // 返回结果：
        ByteBuffer result = ByteBuffer.allocate(byteSize);

        ///* 从本地文件获取文件：
        try{
            //String filePath = "D:\\B881_mid.mp4";
            String filePath = "D:\\TBinaryProtocol.tcc";
            File file = new File(filePath);
            InputStream inputStream = new FileInputStream(file);

            // 获取当前需要读入文件的位置：
            long start = 0;
            inputStream.skip(start);
            // 从指定位置获取文件内容：
            inputStream.read(buffer, 0, buffer.length);
            // 将获取到的内容写入缓存中：
            result.put(buffer);
            result.flip();
        }catch (Exception e){
            e.printStackTrace();
        }
        //*/

        /* 从文件接口获取文件：
        try{
            // 获取基本的操作接口：
            FileMetadataService fileMetadataService = new FileMetadataService();
            FileServiceImpl fileServiceImpl = new FileServiceImpl();
            // 从元数据库获取文件信息：
            FileMetadata fileMetadata = fileMetadataService.findById(dbname, tableName, _id);
            // 判断文件是否存在：
            if (null == fileMetadata) {
                System.out.println("不存在的文件");
                return null;
            }
            // 获取文件大小：
            final long fileLen = fileMetadata.getFileSize();
            // 获取文件流内容：
            InputStream inputStream = fileServiceImpl.getFile(dbname, tableName, _id);
            // 获取当前需要读入文件的位置：
            long start = 0;
            inputStream.skip(start);
            // 从指定位置获取文件内容：
            inputStream.read(buffer, 0, buffer.length);
            // 将获取到的内容写入缓存中：
            result.put(buffer);
            result.flip();
        }catch (Exception e){
            e.printStackTrace();
        }
        //*/

        System.out.println(result.toString());
        return result;
    }

    ///*
    // 测试入口：
    public static void main(String[] args){


        SearchDataResponse result = new SearchDataResponse();
        try{
            com.iscas.datasong.lib.response.data.SearchDataResponse originalBeanResult =
                    new com.iscas.datasong.lib.response.data.SearchDataResponse();
            originalBeanResult.setDBName("dbname");
            originalBeanResult.setTableName("tablename");
            originalBeanResult.setInfo("info");
            originalBeanResult.setTookInMillis(1000);
            originalBeanResult.setTotalHit(1000);
            // 填写列表内容：
            /*
            List<com.iscas.datasong.lib.common.DataItem> listcontent = new ArrayList<>();
            for(int i=0;i<10;i++){
                com.iscas.datasong.lib.common.DataItem tmp = new com.iscas.datasong.lib.common.DataItem();
                tmp.setDBName(String.valueOf(i));
                tmp.setTableName(String.valueOf(i));
                tmp.setId(String.valueOf(i));
                tmp.setScore(i);
                tmp.setVersion(i);
                tmp.setSource(String.valueOf(i));
                listcontent.add(tmp);
            }
            originalBeanResult.setItems(listcontent);
            */

            /////////////////////////////////////////////////////////////////////
            BeanPropertyCopy2(originalBeanResult, result);

            //BeanUtils.copyProperties(result,originalBeanResult);

            //ObjectMapper mapper = new ObjectMapper();
            //result = mapper.convertValue(originalBeanResult, SearchDataResponse.class);
            /////////////////////////////////////////////////////////////////////


            System.out.println(result.getDBName());
            System.out.println(result.getTableName());
            System.out.println(result.getInfo());
            List<DataItem> resultListContent =  result.getItems();
            for(Iterator<DataItem> itemIterator = resultListContent.iterator();itemIterator.hasNext();){
                DataItem currentDataItem = itemIterator.next();
                System.out.println(currentDataItem.getTableName());
            }

        }catch (Exception e){
            e.printStackTrace();
        }



        try {
            TServerSocket serverTransport = new TServerSocket(7911);
            TProcessor processor = new DataSongRequestService.Processor(new requestorServiceImpl());
            TServer server = new TSimpleServer(new TServer.Args(serverTransport).processor(processor));
            System.out.println("Start server on port 7911...");
            server.serve();
        } catch (TTransportException e) {
            e.printStackTrace();
        }
    }
    //*/
}
