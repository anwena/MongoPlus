package com.anwen.mongo.config;

import com.anwen.mongo.cache.global.MongoClientCache;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.interceptor.LogInterceptor;
import com.anwen.mongo.mapper.MongoPlusMapMapper;
import com.anwen.mongo.property.MongoDBCollectionProperty;
import com.anwen.mongo.property.MongoDBConnectProperty;
import com.anwen.mongo.property.MongoDBLogProperty;
import com.anwen.mongo.toolkit.MongoCollectionUtils;
import com.anwen.mongo.toolkit.UrlJoint;
import com.mongodb.ConnectionString;
import com.mongodb.MongoClientSettings;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import org.noear.solon.annotation.Bean;
import org.noear.solon.annotation.Condition;
import org.noear.solon.annotation.Configuration;
import org.noear.solon.annotation.Inject;

import java.util.Optional;

/**
 * @author JiaChaoYang
 * 连接配置
 * @since 2023-02-09 14:27
 **/
@Configuration
public class MongoPlusConfiguration {

    private SqlExecute sqlExecute;

    private MongoClient mongoClient;

    public SqlExecute getSqlExecute() {
        return sqlExecute;
    }

    @Bean
    @Condition(onMissingBean = SqlExecute.class)
    public SqlExecute sqlExecute(@Inject("${mongo-plus.data.mongodb}") MongoDBConnectProperty mongoDBConnectProperty,
                                 @Inject("${mongo-plus}") MongoDBLogProperty mongoDBLogProperty,
                                 @Inject(value = "${mongo-plus.configuration.collection}",required = false) MongoDBCollectionProperty mongoDBCollectionProperty) {
        if (this.sqlExecute != null) {
            return this.sqlExecute;
        }
        mongoDBCollectionProperty = Optional.ofNullable(mongoDBCollectionProperty).orElseGet(MongoDBCollectionProperty::new);
        SqlExecute sqlExecute = new SqlExecute();
        sqlExecute.setSlaveDataSources(mongoDBConnectProperty.getSlaveDataSource());
        sqlExecute.setBaseProperty(mongoDBConnectProperty);
        CollectionNameConvert collectionNameConvert =
                MongoCollectionUtils.build(mongoDBCollectionProperty.getMappingStrategy());
        sqlExecute.setCollectionNameConvert(collectionNameConvert);
        UrlJoint urlJoint = new UrlJoint(mongoDBConnectProperty);
        MongoClientSettings.Builder builder = MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(urlJoint.jointMongoUrl()));
        if (mongoDBLogProperty.getLog()) {
            builder.addCommandListener(new LogInterceptor());
        }
        this.mongoClient = MongoClients.create(builder.build());
        MongoClientCache.mongoClient = this.mongoClient;
        sqlExecute.setMongoClient(this.mongoClient);
        this.sqlExecute = sqlExecute;
        return sqlExecute;
    }

    @Bean("mongo")
    @Condition(onMissingBean = MongoClient.class)
    public MongoClient mongo() {
        return this.mongoClient;
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
