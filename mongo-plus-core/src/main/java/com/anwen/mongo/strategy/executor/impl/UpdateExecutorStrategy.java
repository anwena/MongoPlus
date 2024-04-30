package com.anwen.mongo.strategy.executor.impl;

import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.model.MutablePair;
import com.anwen.mongo.strategy.executor.MethodExecutorStrategy;
import com.mongodb.client.MongoCollection;
import org.bson.Document;
import org.bson.conversions.Bson;

public class UpdateExecutorStrategy implements MethodExecutorStrategy {

    @Override
    public ExecuteMethodEnum method() {
        return ExecuteMethodEnum.UPDATE;
    }

    @Override
    public void invoke(Interceptor interceptor, Object[] args) {
        MutablePair<Bson, Bson> bsonBsonMutablePair = interceptor.executeUpdate((Bson) args[0], (Bson) args[1]);
        args[0] = bsonBsonMutablePair.getLeft();
        args[1] = bsonBsonMutablePair.getRight();
        bsonBsonMutablePair = interceptor.executeUpdate((Bson) args[0], (Bson) args[1], (MongoCollection<Document>) args[2]);
        args[0] = bsonBsonMutablePair.getLeft();
        args[1] = bsonBsonMutablePair.getRight();
    }

}
