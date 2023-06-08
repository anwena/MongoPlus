package com.anwen.mongo.config;

import com.anwen.mongo.sql.model.BaseProperty;
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
public class MongoDBConnectProperty extends BaseProperty {

    /**
     * 从数据源
     * @author: JiaChaoYang
     * @date: 2023/2/18 15:03
     **/
    private List<SlaveDataSource> slaveDataSource;

}
