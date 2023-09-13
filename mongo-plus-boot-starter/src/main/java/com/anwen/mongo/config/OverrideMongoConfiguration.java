package com.anwen.mongo.config;

import com.mongodb.MongoClientSettings;
import com.mongodb.client.MongoClient;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.mongo.MongoAutoConfiguration;
import org.springframework.boot.autoconfigure.mongo.MongoClientSettingsBuilderCustomizer;
import org.springframework.context.annotation.DependsOn;

/**
 * 覆盖MongoTemplate的MongoClient
 * @author JiaChaoYang
 **/
@DependsOn("sqlExecute")
public class OverrideMongoConfiguration extends MongoAutoConfiguration {

    private final MongoClient mongoClient;

    public OverrideMongoConfiguration(MongoClient mongo){
        this.mongoClient = mongo;
    }

    @Override
    public MongoClient mongo(ObjectProvider<MongoClientSettingsBuilderCustomizer> builderCustomizers, MongoClientSettings settings) {
        return this.mongoClient;
    }

}

