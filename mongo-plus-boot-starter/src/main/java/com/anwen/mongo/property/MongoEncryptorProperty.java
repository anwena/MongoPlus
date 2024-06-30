package com.anwen.mongo.property;

import com.anwen.mongo.cache.global.PropertyCache;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * 加解密全局配置
 *
 * @author anwen
 * @date 2024/6/29 下午1:49
 */
@Configuration
@ConfigurationProperties(prefix = "mongo-plus.encryptor")
public class MongoEncryptorProperty {

    /**
     * 私钥，非对称加密
     * @date 2024/6/30 下午1:28
     */
    private String privateKey;

    /**
     * 公钥，非对称加密
     * @date 2024/6/30 下午1:28
     */
    private String publicKey;

    /**
     * 秘钥，对称加密
     * @date 2024/6/30 下午1:28
     */
    private String key;

    public String getPrivateKey() {
        return privateKey;
    }

    public void setPrivateKey(String privateKey) {
        PropertyCache.privateKey = privateKey;
        this.privateKey = privateKey;
    }

    public String getPublicKey() {
        return publicKey;
    }

    public void setPublicKey(String publicKey) {
        PropertyCache.publicKey = publicKey;
        this.publicKey = publicKey;
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        PropertyCache.key = key;
        this.key = key;
    }
}
