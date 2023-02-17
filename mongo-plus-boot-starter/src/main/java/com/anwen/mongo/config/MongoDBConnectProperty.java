package com.anwen.mongo.config;

import com.anwen.mongo.sql.IService;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;

import javax.annotation.Resource;

/**
 * @author JiaChaoYang
 * 属性文件配置
 * @since 2023-02-09 14:29
 **/
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@ConfigurationProperties(prefix = "spring.data.mongodb")
public class MongoDBConnectProperty {

    /**
     * mongodb地址
     * @author JiaChaoYang
     * @since 2023/2/10 13:45
    */
    private String host;

    /**
     * mongodb端口
     * @since 2023/2/10 13:45
    */
    private String port;

    /**
     * TODO mongodb数据库名称，第二版本需要多数据源，可随时切换
     * @since 2023/2/10 13:45
    */
    private String database;

}
