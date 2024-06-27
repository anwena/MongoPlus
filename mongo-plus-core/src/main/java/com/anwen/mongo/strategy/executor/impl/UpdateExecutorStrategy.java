package com.anwen.mongo.strategy.executor.impl;

import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.model.MutablePair;
import com.anwen.mongo.strategy.executor.MethodExecutorStrategy;
import com.mongodb.client.MongoCollection;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.util.List;

/**
 * UPDATE 策略执行器
 *
 * @author loser
 * @date 2024/4/30
 */
@SuppressWarnings("unchecked")
public class UpdateExecutorStrategy implements MethodExecutorStrategy {

    @Override
    public ExecuteMethodEnum method() {
        return ExecuteMethodEnum.UPDATE;
    }

    @Override
    public void invoke(Interceptor interceptor, Object[] args) {
        List<MutablePair<Bson, Bson>> bsonBsonMutablePairList = interceptor.executeUpdate((List<MutablePair<Bson,Bson>>) args[0]);
        args[0] = bsonBsonMutablePairList;
        bsonBsonMutablePairList = interceptor.executeUpdate((List<MutablePair<Bson,Bson>>) args[0], (MongoCollection<Document>) args[args.length-1]);
        args[0] = bsonBsonMutablePairList;
    }

}
