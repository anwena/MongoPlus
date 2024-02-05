package com.anwen.mongo.interceptor;

import com.anwen.mongo.model.AggregateBasicDBObject;
import com.mongodb.BasicDBObject;
import com.mongodb.client.model.CountOptions;
import com.mongodb.client.model.WriteModel;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.util.List;

/**
 * 拦截器
 *
 * @author JiaChaoYang
 **/
public interface Interceptor {

    void executeSave(List<Document> documentList);

    void executeRemove(Bson filter);

    void executeUpdate(Bson queryBasic, Bson updateBasic);

    void executeQuery(Bson queryBasic, BasicDBObject projectionList, BasicDBObject sortCond);

    void executeAggregate(List<AggregateBasicDBObject> aggregateConditionList);

    void executeCount(BasicDBObject queryBasic, CountOptions countOptions);

    void executeBulkWrite(List<WriteModel<Document>> writeModelList);

}
