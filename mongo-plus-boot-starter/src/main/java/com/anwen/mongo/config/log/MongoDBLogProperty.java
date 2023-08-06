package com.anwen.mongo.config.log;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * @author JiaChaoYang
 * 日志属性
 * @since 2023-06-07 23:07
 **/
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@ConfigurationProperties(prefix = "mongo-plus")
public class MongoDBLogProperty {

    /**
     * 是否开启日志
     * @author: JiaChaoYang
     * @date: 2023/6/7 23:08
     **/
    private Boolean log;

}
