package com.anwen.mongo.config;

import com.anwen.mongo.model.BaseProperty;
import com.anwen.mongo.model.SlaveDataSource;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.noear.solon.annotation.Configuration;
import org.noear.solon.annotation.Inject;

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
@Configuration
@Inject("${mongo-plus.data.mongodb}")
public class MongoDBConnectProperty extends BaseProperty {

    /**
     * 从数据源
     * @author: JiaChaoYang
     * @date: 2023/2/18 15:03
     **/
    private List<SlaveDataSource> slaveDataSource;

}
