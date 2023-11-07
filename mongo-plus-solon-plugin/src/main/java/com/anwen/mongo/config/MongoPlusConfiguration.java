package com.anwen.mongo.config;

import com.anwen.mongo.property.MongoDBConnectProperty;
import com.anwen.mongo.property.MongoDBLogProperty;
import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.log.CustomMongoDriverLogger;
import com.anwen.mongo.mapper.MongoPlusMapMapper;
import com.anwen.mongo.toolkit.UrlJoint;
import com.mongodb.ConnectionString;
import com.mongodb.MongoClientSettings;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import org.noear.solon.annotation.Bean;
import org.noear.solon.annotation.Condition;
import org.noear.solon.annotation.Configuration;
import org.noear.solon.annotation.Inject;

/**
 * @author JiaChaoYang
 * 连接配置
 * @since 2023-02-09 14:27
 **/
@Configuration
public class MongoPlusConfiguration {

    private SqlExecute sqlExecute;

    public SqlExecute getSqlExecute() {
        return sqlExecute;
    }

    @Bean
    @Condition(onMissingBean = SqlExecute.class)
    public SqlExecute sqlExecute(@Inject("${mongo-plus.data.mongodb}") MongoDBConnectProperty mongoDBConnectProperty, @Inject("${mongo-plus}") MongoDBLogProperty mongoDBLogProperty) {
        if (this.sqlExecute != null){
            return this.sqlExecute;
        }
        SqlExecute sqlExecute = new SqlExecute();
        sqlExecute.setSlaveDataSources(mongoDBConnectProperty.getSlaveDataSource());
        sqlExecute.setBaseProperty(mongoDBConnectProperty);
        UrlJoint urlJoint = new UrlJoint(mongoDBConnectProperty);
        MongoClientSettings.Builder builder = MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(urlJoint.jointMongoUrl()));
        if (mongoDBLogProperty.getLog()){
            builder.addCommandListener(new CustomMongoDriverLogger(mongoDBLogProperty.getFormat()));
        }
        MongoClient mongoClient = MongoClients.create(builder.build());
        sqlExecute.setMongoClient(mongoClient);
        this.sqlExecute = sqlExecute;
        return sqlExecute;
    }

    @Bean
    @Condition(onMissingBean = MongoPlusMapMapper.class)
    public MongoPlusMapMapper mongoPlusMapMapper(@Inject SqlExecute sqlExecute){
        return new MongoPlusMapMapper(sqlExecute);
    }

    @Bean
    public MongoPlusAutoConfiguration mongoPlusAutoConfiguration(@Inject SqlExecute sqlExecute){
        return new MongoPlusAutoConfiguration(this.sqlExecute);
    }

}