package com.anwen.mongo.config.log;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import com.anwen.mongo.log.CustomMongoDriverLogger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;


/**
 * @author JiaChaoYang
 * 日志配置
 * @since 2023-06-07 23:00
 **/
@Configuration
@EnableConfigurationProperties(MongoDBLogProperty.class)
public class MongoLogConfig {

    public MongoLogConfig(MongoDBLogProperty mongoDBLogProperty) {
        setMongoDBLogLevel(mongoDBLogProperty);
    }

    private void setMongoDBLogLevel(MongoDBLogProperty mongoDBLogProperty) {
        Logger logger = (Logger) LoggerFactory.getLogger(CustomMongoDriverLogger.class);
        String level = "INFO";
        if (mongoDBLogProperty.getLog()){
            level = "DEBUG";
        }
        logger.setLevel(Level.toLevel(level,Level.INFO));
    }

}
