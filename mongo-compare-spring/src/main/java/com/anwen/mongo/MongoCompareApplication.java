package com.anwen.mongo;

import com.anwen.mongo.config.MongoPropertyConfiguration;
import com.anwen.mongo.config.OverrideMongoConfiguration;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;


@SpringBootApplication(exclude = {
        OverrideMongoConfiguration.class,
        MongoPropertyConfiguration.class
})  //排除对MongoTemplate的一些配置，以防影响到MongoTemplate
public class MongoCompareApplication {
    public static void main(String[] args) {
        SpringApplication.run(MongoCompareApplication.class, args);
    }
}