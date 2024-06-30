package com.anwen.mongo.encryptor;

/**
 * 加密接口
 * @author anwen
 * @date 2024/6/30 上午1:16
 */
public interface Encryptor {

    /**
     * 加密
     * @param data 明文
     * @param key key
     * @param publicKey 公钥
     * @return {@link java.lang.String}
     * @author anwen
     * @date 2024/6/30 上午1:17
     */
    String encrypt(String data,String key,String publicKey) throws Exception;

    /**
     * 解密
     *
     * @param data       密文
     * @param key        key
     * @param privateKey 私钥
     * @return {@link java.lang.String}
     * @author anwen
     * @date 2024/6/30 上午1:17
     */
    String decrypt(String data,String key,String privateKey) throws Exception;

}
