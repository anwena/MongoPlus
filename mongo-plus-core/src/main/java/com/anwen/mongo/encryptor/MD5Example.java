package com.anwen.mongo.encryptor;

import com.anwen.mongo.toolkit.StringUtils;

import java.security.MessageDigest;

/**
 * MD5加密
 *
 * @author anwen
 * @date 2024/6/29 下午1:07
 */
public class MD5Example implements Encryptor {

    private final Integer size;

    public MD5Example(Integer size) {
        this.size = size;
    }

    public MD5Example() {
        size = 32;
    }

    @Override
    public String encrypt(String data, String key,String publicKey) throws Exception {
        // 获取MD5加密实例
        MessageDigest md = MessageDigest.getInstance("MD5");

        // 将原始字符串转换为字节数组
        byte[] originalBytes = data.getBytes();

        // 计算MD5哈希值
        byte[] hashBytes = md.digest(originalBytes);
        String hex = StringUtils.bytesToHex(hashBytes);
        return size == 32 ? hex : hex.substring(8, 24);
    }

    @Override
    public String decrypt(String data, String key, String privateKey) throws Exception {
        return data;
    }
}
