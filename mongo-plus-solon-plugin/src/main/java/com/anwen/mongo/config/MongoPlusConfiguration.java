package com.anwen.mongo.config;

import com.anwen.mongo.config.log.MongoDBLogProperty;
import com.anwen.mongo.execute.SqlOperation;
import com.anwen.mongo.log.CustomMongoDriverLogger;
import com.anwen.mongo.mapper.MongoPlusMapMapper;
import com.anwen.mongo.toolkit.UrlJoint;
import com.mongodb.ConnectionString;
import com.mongodb.MongoClientSettings;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import lombok.extern.log4j.Log4j2;
import org.noear.solon.annotation.Bean;
import org.noear.solon.annotation.Configuration;
import org.noear.solon.annotation.Inject;

import javax.annotation.PostConstruct;
import java.util.Map;

/**
 * @author JiaChaoYang
 * 连接配置
 * @since 2023-02-09 14:27
 **/
@Log4j2
@Configuration
public class MongoPlusConfiguration {

    /**
     * 自定义事件
     * @author JiaChaoYang
     * @date 2023/6/26/026 22:06
     */

    @Inject
    private MongoDBLogProperty mongoDBLogProperty;

    @Inject
    private MongoDBConnectProperty mongoDBConnectProperty;

    private MongoClient mongoClient;

    private SqlOperation sqlOperation;

    public MongoPlusConfiguration(MongoDBConnectProperty mongoDBConnectProperty,MongoDBLogProperty mongoDBLogProperty) {
        this.mongoDBConnectProperty = mongoDBConnectProperty;
        this.mongoDBLogProperty = mongoDBLogProperty;
    }

    @Bean
    @PostConstruct
    public SqlOperation sqlOperation() {
        if (sqlOperation != null){
            return this.sqlOperation;
        }
        SqlOperation sqlOperation = new SqlOperation();
        sqlOperation.setSlaveDataSources(mongoDBConnectProperty.getSlaveDataSource());
        sqlOperation.setBaseProperty(mongoDBConnectProperty);
        UrlJoint urlJoint = new UrlJoint(mongoDBConnectProperty);
        MongoClientSettings.Builder builder = MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(urlJoint.jointMongoUrl()));
        if (mongoDBLogProperty.getLog()){
            builder.addCommandListener(new CustomMongoDriverLogger(mongoDBLogProperty.getLog()));
        }
        this.mongoClient = MongoClients.create(builder.build());
        sqlOperation.setMongoClient(this.mongoClient);
        this.sqlOperation = sqlOperation;
        return sqlOperation;
    }

    @Bean
    public MongoPlusMapMapper mongoPlusMapMapper(SqlOperation sqlOperation){
        return new MongoPlusMapMapper(sqlOperation);
    }

}
