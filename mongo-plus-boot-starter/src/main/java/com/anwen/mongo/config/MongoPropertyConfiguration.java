/*
 * Copyright (c) JiaChaoYang 2024-1-24 MongoPlus版权所有
 * 适度编码益脑，沉迷编码伤身，合理安排时间，享受快乐生活。
 * email: j15030047216@163.com
 * phone: 15030047216
 * weChat: JiaChaoYang_
 */

package com.anwen.mongo.config;

import com.anwen.mongo.property.MongoDBConnectProperty;
import com.anwen.mongo.toolkit.StringUtils;
import org.springframework.boot.autoconfigure.mongo.MongoProperties;
import org.springframework.context.annotation.Configuration;

import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * @author JiaChaoYang
 **/
@Configuration
public class MongoPropertyConfiguration {

    private final MongoDBConnectProperty mongoDBConnectProperty;

    private final MongoProperties mongoProperties;

    public MongoPropertyConfiguration(MongoDBConnectProperty mongoDBConnectProperty, MongoProperties mongoProperties) {
        this.mongoDBConnectProperty = mongoDBConnectProperty;
        this.mongoProperties = mongoProperties;
        init();
    }

    public void init(){
        if (StringUtils.isNotBlank(mongoDBConnectProperty.getUrl())){
            mongoProperties.setUri(mongoDBConnectProperty.getUrl());
        }else {
            mongoProperties.setHost(Arrays.stream(mongoDBConnectProperty.getHost().split(",")).collect(Collectors.toList()).get(0));
            mongoProperties.setPort(Integer.valueOf(Arrays.stream(mongoDBConnectProperty.getPort().split(",")).collect(Collectors.toList()).get(0)));
            if (StringUtils.isNotBlank(mongoDBConnectProperty.getUsername())) {
                mongoProperties.setUsername(mongoDBConnectProperty.getUsername());
            }
            if (StringUtils.isNotBlank(mongoDBConnectProperty.getPassword())) {
                mongoProperties.setPassword(mongoDBConnectProperty.getPassword().toCharArray());
            }
            mongoProperties.setAuthenticationDatabase(mongoDBConnectProperty.getAuthenticationDatabase());
        }
        mongoProperties.setDatabase(Arrays.stream(mongoDBConnectProperty.getDatabase().split(",")).collect(Collectors.toList()).get(0));
    }

}
