package com.anwen.mongo.config;

import com.anwen.mongo.config.log.MongoDBLogProperty;
import com.anwen.mongo.event.SqlOperationInitializedEvent;
import com.anwen.mongo.log.CustomMongoDriverLogger;
import com.anwen.mongo.sql.MongoPlusBaseMapper;
import com.anwen.mongo.sql.SqlOperation;
import com.anwen.mongo.sql.inject.query.MongoPlusOperate;
import com.anwen.mongo.transactional.MongoTransactionalAspect;
import com.anwen.mongo.utils.UrlJoint;
import com.mongodb.ConnectionString;
import com.mongodb.MongoClientSettings;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import com.mongodb.event.CommandListener;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.mongo.MongoAutoConfiguration;
import org.springframework.boot.autoconfigure.mongo.MongoClientSettingsBuilderCustomizer;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.Bean;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author JiaChaoYang
 * 连接配置
 * @since 2023-02-09 14:27
 **/
@Log4j2
@EnableConfigurationProperties({MongoDBConnectProperty.class, MongoDBLogProperty.class})
public class MongoPlusConfiguration extends MongoAutoConfiguration{

    /**
     * 自定义事件
     * @author JiaChaoYang
     * @date 2023/6/26/026 22:06
     */
    @Resource
    private ApplicationEventPublisher eventPublisher;

    private final MongoDBLogProperty mongoDBLogProperty;

    final
    MongoDBConnectProperty mongoDBConnectProperty;

    private MongoClient mongoClient;

    @Override
    public MongoClient mongo(ObjectProvider<MongoClientSettingsBuilderCustomizer> builderCustomizers, MongoClientSettings settings) {
        return this.mongoClient;
    }


    public MongoPlusConfiguration(MongoDBConnectProperty mongoDBConnectProperty,MongoDBLogProperty mongoDBLogProperty) {
        this.mongoDBConnectProperty = mongoDBConnectProperty;
        this.mongoDBLogProperty = mongoDBLogProperty;
    }

    @Bean
    @ConditionalOnMissingBean
    @PostConstruct
    public SqlOperation<?> sqlOperation() {
        SqlOperation<?> sqlOperation = new SqlOperation<>();
        sqlOperation.setSlaveDataSources(mongoDBConnectProperty.getSlaveDataSource());
        sqlOperation.setBaseProperty(mongoDBConnectProperty);
        UrlJoint urlJoint = new UrlJoint(mongoDBConnectProperty);
        MongoClientSettings.Builder builder = MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(urlJoint.jointMongoUrl()));
        if (mongoDBLogProperty.getLog()){
            builder.addCommandListener(new CustomMongoDriverLogger());
        }
        this.mongoClient = MongoClients.create(builder.build());
        sqlOperation.setMongoClient(this.mongoClient);
        // 发布自定义事件通知其他类，sqlOperation已完成初始化
        eventPublisher.publishEvent(new SqlOperationInitializedEvent(sqlOperation));
        return sqlOperation;
    }

    @Bean
    public MongoPlusOperate mongoPlusOperate(SqlOperation<Map<String,Object>> sqlOperation){
        return new MongoPlusOperate(sqlOperation);
    }

    @Bean
    public MongoPlusBaseMapper<?> mongoPlusBaseMapper(){
        return new MongoPlusBaseMapper<>();
    }

    @Bean
    public MongoTransactionalAspect mongoTransactionalAspect(){
        return new MongoTransactionalAspect(mongoClient);
    }

}
