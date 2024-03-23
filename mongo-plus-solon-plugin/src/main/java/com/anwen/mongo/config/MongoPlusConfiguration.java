package com.anwen.mongo.config;

import com.anwen.mongo.cache.global.MongoPlusClientCache;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.listener.BaseListener;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.mapper.DefaultBaseMapperImpl;
import com.anwen.mongo.mapper.MongoPlusMapMapper;
import com.anwen.mongo.property.MongoDBCollectionProperty;
import com.anwen.mongo.property.MongoDBConfigurationProperty;
import com.anwen.mongo.property.MongoDBConnectProperty;
import com.anwen.mongo.property.MongoDBLogProperty;
import com.anwen.mongo.toolkit.MongoCollectionUtils;
import com.anwen.mongo.toolkit.UrlJoint;
import com.anwen.mongo.transactional.MongoTransactionalAspect;
import com.mongodb.ConnectionString;
import com.mongodb.MongoClientSettings;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import org.noear.solon.annotation.Bean;
import org.noear.solon.annotation.Condition;
import org.noear.solon.annotation.Configuration;
import org.noear.solon.annotation.Inject;

import java.util.Collections;
import java.util.Optional;

/**
 * @author JiaChaoYang
 * 连接配置
 * @since 2023-02-09 14:27
 **/
@Configuration
public class MongoPlusConfiguration {

    @Inject("${mongo-plus.data.mongodb}")
    private MongoDBConnectProperty mongoDBConnectProperty;

    @Inject(value = "${mongo-plus.configuration.collection}",required = false)
    private MongoDBCollectionProperty mongoDBCollectionProperty;

    @Inject(value = "${mongo-plus.configuration}",required = false)
    private MongoDBConfigurationProperty mongoDBConfigurationProperty;

    /**
     * 将MongoClient注册为Bean
     * @author JiaChaoYang
     * @date 2024/1/4 23:49
     */
    @Bean
    @Condition(onMissingBean = MongoClient.class)
    public MongoClient mongo(){
        return MongoClients.create(MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(new UrlJoint(mongoDBConnectProperty).jointMongoUrl())).commandListenerList(Collections.singletonList(new BaseListener())).build());
    }

    @Bean
    @Condition(onMissingBean = CollectionNameConvert.class)
    public CollectionNameConvert collectionNameConvert(){
        mongoDBCollectionProperty = Optional.ofNullable(mongoDBCollectionProperty).orElseGet(MongoDBCollectionProperty::new);
        return MongoCollectionUtils.build(mongoDBCollectionProperty.getMappingStrategy());
    }

    @Bean
    @Condition(onMissingBean = MongoPlusClient.class)
    public MongoPlusClient mongoPlusClient(MongoClient mongo,CollectionNameConvert collectionNameConvert){
        mongoDBConfigurationProperty = Optional.ofNullable(mongoDBConfigurationProperty).orElseGet(MongoDBConfigurationProperty::new);
        MongoPlusClient mongoPlusClient = com.anwen.mongo.config.Configuration.builder().initMongoPlusClient(mongo, collectionNameConvert, mongoDBConnectProperty);
        MongoPlusClientCache.mongoPlusClient = mongoPlusClient;
        if (mongoDBConfigurationProperty.getBanner()){
            System.out.println("___  ___                       ______ _           \n" +
                    "|  \\/  |                       | ___ \\ |          \n" +
                    "| .  . | ___  _ __   __ _  ___ | |_/ / |_   _ ___ \n" +
                    "| |\\/| |/ _ \\| '_ \\ / _` |/ _ \\|  __/| | | | / __|\n" +
                    "| |  | | (_) | | | | (_| | (_) | |   | | |_| \\__ \\\n" +
                    "\\_|  |_/\\___/|_| |_|\\__, |\\___/\\_|   |_|\\__,_|___/\n" +
                    "                     __/ |                        \n" +
                    "                    |___/                         ");
        }
        return mongoPlusClient;
    }

    @Bean
    @Condition(onMissingBean = MongoPlusMapMapper.class)
    public MongoPlusMapMapper mongoPlusMapMapper(MongoPlusClient mongoPlusClient) {
        return new MongoPlusMapMapper(mongoPlusClient);
    }

    @Bean("mongoTransactionalAspect")
    @Deprecated
    @Condition(onMissingBean = MongoTransactionalAspect.class)
    public MongoTransactionalAspect mongoTransactionalAspect(MongoClient mongoClient) {
        return new MongoTransactionalAspect(mongoClient);
    }

    @Bean
    public MongoPlusAutoConfiguration mongoPlusAutoConfiguration(@Inject BaseMapper baseMapper,
                                                                 @Inject MongoPlusClient mongoPlusClient,
                                                                 @Inject CollectionNameConvert collectionNameConvert,
                                                                 @Inject("${mongo-plus}") MongoDBLogProperty mongoDBLogProperty){
        return new MongoPlusAutoConfiguration(baseMapper,mongoPlusClient,collectionNameConvert,mongoDBLogProperty,mongoDBCollectionProperty);
    }

    @Bean
    @Condition(onMissingBean = BaseMapper.class)
    public BaseMapper baseMapper(MongoPlusClient mongoPlusClient){
        return new DefaultBaseMapperImpl(mongoPlusClient);
    }

}
