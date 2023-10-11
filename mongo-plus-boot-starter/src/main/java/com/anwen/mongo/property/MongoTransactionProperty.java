package com.anwen.mongo.property;

import com.anwen.mongo.cache.PropertyCache;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description
 * @date 2023-10-10 13:07
 **/
@ConfigurationProperties(prefix = "mongo-plus.spring")
public class MongoTransactionProperty {

    private Boolean transaction = false;

    public Boolean getTransaction() {
        return transaction;
    }

    public void setTransaction(Boolean transaction) {
        PropertyCache.transaction = transaction;
        this.transaction = transaction;
    }

}
