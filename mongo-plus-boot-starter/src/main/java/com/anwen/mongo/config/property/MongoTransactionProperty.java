package com.anwen.mongo.config.property;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description
 * @date 2023-10-10 13:07
 **/
@ConfigurationProperties(prefix = "mongo-plus.spring")
public class MongoTransactionProperty {

    public Boolean getTransaction() {
        return transaction;
    }

    public void setTransaction(Boolean transaction) {
        this.transaction = transaction;
    }

    private Boolean transaction = false;

}
