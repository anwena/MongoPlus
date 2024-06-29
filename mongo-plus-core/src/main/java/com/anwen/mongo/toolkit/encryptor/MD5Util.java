package com.anwen.mongo.toolkit.encryptor;

import com.anwen.mongo.domain.MongoPlusEncryptException;
import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;

import java.security.MessageDigest;

/**
 * MD5加密
 *
 * @author anwen
 * @date 2024/6/29 下午1:07
 */
public class MD5Util {

    private final static Log log = LogFactory.getLog(MD5Util.class);

    /**
     * 加密为32为md5
     * @param original 原文
     * @author anwen
     * @date 2024/6/29 下午1:16
     */
    public static String md5_32(String original){
        try {
            // 获取MD5加密实例
            MessageDigest md = MessageDigest.getInstance("MD5");

            // 将原始字符串转换为字节数组
            byte[] originalBytes = original.getBytes();

            // 计算MD5哈希值
            byte[] hashBytes = md.digest(originalBytes);

            // 将字节数组转换为十六进制字符串
            StringBuilder hexString = new StringBuilder();
            for (byte hashByte : hashBytes) {
                String hex = Integer.toHexString(0xff & hashByte);
                if (hex.length() == 1) {
                    hexString.append('0');
                }
                hexString.append(hex);
            }
            return hexString.toString();
        } catch (Exception e) {
            log.error("MD5 encryption failed,message: {}",e.getMessage(),e);
            throw new MongoPlusEncryptException("32-bit MD5 encryption failed",e);
        }
    }

    /**
     * 加密为16为md5
     * @param original 原文
     * @return {@link java.lang.String}
     * @author anwen
     * @date 2024/6/29 下午1:16
     */
    public static String md5_16(String original) {
        return md5_32(original).substring(8, 24);
    }

    public static void main(String[] args) {
        System.out.println(md5_32("1"));
    }

}
