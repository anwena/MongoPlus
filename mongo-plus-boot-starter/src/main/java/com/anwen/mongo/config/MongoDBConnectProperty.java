package com.anwen.mongo.config;

import com.anwen.mongo.sql.model.SlaveDataSource;
import lombok.*;
import org.springframework.boot.context.properties.ConfigurationProperties;

import java.util.List;

/**
 * @author JiaChaoYang
 * 属性文件配置
 * @since 2023-02-09 14:29
 **/
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@ConfigurationProperties(prefix = "mongo-plus.data.mongodb")
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

    /**
     * 从数据源
     * @author: JiaChaoYang
     * @date: 2023/2/18 15:03
     **/
    private List<SlaveDataSource> slaveDataSource;

}
