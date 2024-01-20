package com.anwen.mongo.property;

import com.anwen.mongo.model.BaseProperty;
import com.anwen.mongo.model.SlaveDataSource;
import org.noear.solon.annotation.Configuration;

import java.util.List;

/**
 * @author JiaChaoYang
 * 属性文件配置
 * @since 2023-02-09 14:29
 **/
@Configuration
public class MongoDBConnectProperty extends BaseProperty {

    /**
     * 从数据源
     * @author: JiaChaoYang
     * @date: 2023/2/18 15:03
     **/
    private List<SlaveDataSource> slaveDataSource;

    public List<SlaveDataSource> getSlaveDataSource() {
        return this.slaveDataSource;
    }

    public void setSlaveDataSource(final List<SlaveDataSource> slaveDataSource) {
        this.slaveDataSource = slaveDataSource;
    }

    public MongoDBConnectProperty(final List<SlaveDataSource> slaveDataSource) {
        this.slaveDataSource = slaveDataSource;
    }

    public MongoDBConnectProperty() {
    }

}
