package com.anwen.mongo.toolkit;

import com.anwen.mongo.domain.MongoPlusConvertException;
import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import org.bson.types.Binary;

import java.io.*;
import java.net.URL;
import java.net.URLConnection;

/**
 * Binary转换工具类
 *
 * @author JiaChaoYang
 **/
public class BinaryConvertHandler {

    private static final Log log = LogFactory.getLog(BinaryConvertHandler.class);

    /**
     * 将File转为Binary
     * @param file 文件
     * @return org.bson.types.Binary
     * @author JiaChaoYang
     * @date 2023/11/24 23:27
    */
    public static Binary fileToBinary(File file){
        try {
            FileInputStream fis = new FileInputStream(file);
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            byte[] buffer = new byte[4096];
            int bytesRead;
            while ((bytesRead = fis.read(buffer)) != -1) {
                bos.write(buffer, 0, bytesRead);
            }
            byte[] fileData = bos.toByteArray();
            Binary binary = new Binary(fileData);
            fis.close();
            bos.close();
            return binary;
        } catch (IOException e) {
            log.error("convert binary error: {}",e.getMessage());
            throw new MongoPlusConvertException(e.getMessage());
        }
    }

    /**
     * fileToBinary(File file)方法的重载
     * @param filePath 本地文件路径
     * @return org.bson.types.Binary
     * @author JiaChaoYang
     * @date 2023/11/24 23:28
    */
    public static Binary fileToBinary(String filePath){
        return fileToBinary(new File(filePath));
    }

    /**
     * 将一个InputStream转为binary
     * @param inputStream 文件流
     * @return org.bson.types.Binary
     * @author JiaChaoYang
     * @date 2023/11/24 23:30
    */
    public static Binary inputStreamToBinary(InputStream inputStream) {
        try {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            byte[] buffer = new byte[4096];
            int bytesRead;
            while ((bytesRead = inputStream.read(buffer)) != -1) {
                bos.write(buffer, 0, bytesRead);
            }
            byte[] data = bos.toByteArray();
            return new Binary(data);
        } catch (IOException e) {
            log.error("convert binary error: {}",e.getMessage());
            throw new MongoPlusConvertException(e.getMessage());
        }
    }

    /**
     * 将一个网络文件转为binary
     * @param url 网络文件地址
     * @return org.bson.types.Binary
     * @author JiaChaoYang
     * @date 2023/11/24 23:30
    */
    public static Binary urlToBinary(String url) {
        try {
            URL fileUrl = new URL(url);
            URLConnection connection = fileUrl.openConnection();
            InputStream inputStream = connection.getInputStream();

            BufferedInputStream bis = new BufferedInputStream(inputStream);
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            byte[] buffer = new byte[4096];
            int bytesRead;
            while ((bytesRead = bis.read(buffer)) != -1) {
                bos.write(buffer, 0, bytesRead);
            }
            byte[] data = bos.toByteArray();
            return new Binary(data);
        } catch (IOException e) {
            log.error("convert binary error: {}",e.getMessage());
            throw new MongoPlusConvertException(e.getMessage());
        }
    }

    /**
     * 将binary转为File类
     * @param binary 二进制数据
     * @param file file类
     * @return java.io.File
     * @author JiaChaoYang
     * @date 2023/11/24 23:45
    */
    public static File binaryConvertFile(Binary binary, File file) throws IOException {
        byte[] data = binary.getData();
        try (FileOutputStream fos = new FileOutputStream(file)) {
            fos.write(data);
        }
        return file;
    }

    /**
     * 将binary保存到本地
     * @author JiaChaoYang
     * @date 2023/11/24 23:34
    */
    public static void saveBinaryToFile(Binary binary, String filePath) {
        try {
            byte[] data = binary.getData();
            FileOutputStream fos = new FileOutputStream(filePath);
            fos.write(data);
            fos.close();
        } catch (IOException e) {
            log.error("save binary error: {}",e.getMessage());
            throw new MongoPlusConvertException(e.getMessage());
        }
    }

}
