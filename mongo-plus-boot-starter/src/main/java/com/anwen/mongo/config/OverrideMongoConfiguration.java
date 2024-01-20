package com.anwen.mongo.config;

import com.anwen.mongo.property.MongoSpringProperty;
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
@DependsOn("mongoClient")
public class OverrideMongoConfiguration extends MongoAutoConfiguration {

    private final MongoClient mongoClient;

    private final MongoSpringProperty mongoSpringProperty;

    public OverrideMongoConfiguration(MongoClient mongoClient, MongoSpringProperty mongoSpringProperty){
        this.mongoClient = mongoClient;
        this.mongoSpringProperty = mongoSpringProperty;
    }

    @Override
    public MongoClient mongo(ObjectProvider<MongoClientSettingsBuilderCustomizer> builderCustomizers, MongoClientSettings settings) {
        if (mongoSpringProperty.getOverrideMongoClient()){
            return super.mongo(builderCustomizers,settings);
        }
        return this.mongoClient;
    }

}

