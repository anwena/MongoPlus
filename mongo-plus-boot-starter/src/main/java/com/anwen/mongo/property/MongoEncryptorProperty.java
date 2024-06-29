package com.anwen.mongo.property;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * 加解密配置
 *
 * @author anwen
 * @date 2024/6/29 下午1:49
 */
@Configuration
@ConfigurationProperties(prefix = "mongo-plus.encryptor")
public class MongoEncryptorProperty {



}
