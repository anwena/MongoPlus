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

/**
 * @author JiaChaoYang
 * 连接配置
 * @since 2023-02-09 14:27
 **/
@Log4j2
@Configuration
public class MongoPlusConfiguration {

    @Bean
    public SqlOperation sqlOperation(@Inject("${mongo-plus.data.mongodb}") MongoDBConnectProperty mongoDBConnectProperty,@Inject("${mongo-plus}") MongoDBLogProperty mongoDBLogProperty) {
        SqlOperation sqlOperation = new SqlOperation();
        sqlOperation.setSlaveDataSources(mongoDBConnectProperty.getSlaveDataSource());
        sqlOperation.setBaseProperty(mongoDBConnectProperty);
        UrlJoint urlJoint = new UrlJoint(mongoDBConnectProperty);
        MongoClientSettings.Builder builder = MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(urlJoint.jointMongoUrl()));
        if (mongoDBLogProperty.getLog()){
            builder.addCommandListener(new CustomMongoDriverLogger(mongoDBLogProperty.getFormat()));
        }
        MongoClient mongoClient = MongoClients.create(builder.build());
        sqlOperation.setMongoClient(mongoClient);
        return sqlOperation;
    }


    @Bean
    public MongoPlusAutoConfiguration mongoPlusAutoConfiguration(@Inject SqlOperation sqlOperation){
        return new MongoPlusAutoConfiguration(sqlOperation);
    }


    @Bean
    public MongoPlusMapMapper mongoPlusMapMapper(@Inject SqlOperation sqlOperation){
        return new MongoPlusMapMapper(sqlOperation);
    }
}
