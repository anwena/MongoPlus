/*
 * Copyright (c) JiaChaoYang 2024-1-24 MongoPlus版权所有
 * 适度编码益脑，沉迷编码伤身，合理安排时间，享受快乐生活。
 * email: j15030047216@163.com
 * phone: 15030047216
 * weChat: JiaChaoYang_
 */

package com.anwen.mongo.config;

import com.anwen.mongo.aware.Aware;
import com.anwen.mongo.logic.LogicNamespaceAware;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * mongoPlus 感知类配置
 *
 * @author loser
 * @date 2024/6/30
 */
@Configuration
public class MongoPlusAwareConfiguration implements ApplicationListener<ApplicationReadyEvent> {

    @Bean
    public LogicNamespaceAware logicNamespaceAware() {
        return new LogicNamespaceAware();
    }

    /**
     * 设置感知类
     *
     * @author loser
     */
    public void setAware(ApplicationContext applicationContext) {

        com.anwen.mongo.config.Configuration builder = com.anwen.mongo.config.Configuration.builder();
        for (Aware aware : applicationContext.getBeansOfType(Aware.class).values()) {
            builder.aware(aware);
        }

    }


    @Override
    public void onApplicationEvent(ApplicationReadyEvent event) {
        setAware(event.getApplicationContext());
    }
    
}
