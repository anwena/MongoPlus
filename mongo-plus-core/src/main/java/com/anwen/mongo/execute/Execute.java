package com.anwen.mongo.execute;

import com.anwen.mongo.model.AggregateBasicDBObject;
import com.anwen.mongo.model.MutablePair;
import com.mongodb.BasicDBObject;
import com.mongodb.bulk.BulkWriteResult;
import com.mongodb.client.AggregateIterable;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.*;
import com.mongodb.client.result.DeleteResult;
import com.mongodb.client.result.InsertManyResult;
import com.mongodb.client.result.UpdateResult;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.util.List;

/**
 * 执行器接口
 *
 * @author JiaChaoYang
 **/
public interface Execute {

    InsertManyResult executeSave(List<Document> documentList, MongoCollection<Document> collection);

    DeleteResult executeRemove(Bson filter, MongoCollection<Document> collection);

    @Deprecated
    UpdateResult executeUpdate(Bson queryBasic,Bson updateBasic,MongoCollection<Document> collection);

    UpdateResult executeUpdate(List<MutablePair<Bson,Bson>> bsonPairList,MongoCollection<Document> collection);

    <T> FindIterable<T> executeQuery(Bson queryBasic, BasicDBObject projectionList, BasicDBObject sortCond, Class<T> clazz, MongoCollection<Document> collection);

    <T> AggregateIterable<T> executeAggregateOld(List<AggregateBasicDBObject> aggregateConditionList, Class<T> clazz, MongoCollection<Document> collection);

    <T> AggregateIterable<T> executeAggregate(List<? extends Bson> aggregateConditionList, Class<T> clazz, MongoCollection<Document> collection);

    long executeCount(BasicDBObject queryBasic,CountOptions countOptions,MongoCollection<Document> collection);

    long estimatedDocumentCount(MongoCollection<Document> collection);

    BulkWriteResult executeBulkWrite(List<WriteModel<Document>> writeModelList, MongoCollection<Document> collection);

    String doCreateIndex(Bson bson,MongoCollection<Document> collection);

    String doCreateIndex(Bson bson,IndexOptions indexOptions,MongoCollection<Document> collection);

    List<String> doCreateIndexes(List<IndexModel> indexes,MongoCollection<Document> collection);

    List<String> doCreateIndexes(List<IndexModel> indexes, CreateIndexOptions createIndexOptions,MongoCollection<Document> collection);

    List<Document> doListIndexes(MongoCollection<Document> collection);

    void doDropIndex(String indexName,MongoCollection<Document> collection);

    void doDropIndex(String indexName,DropIndexOptions dropIndexOptions,MongoCollection<Document> collection);

    void doDropIndex(Bson keys,MongoCollection<Document> collection);

    void doDropIndex(Bson keys,DropIndexOptions dropIndexOptions,MongoCollection<Document> collection);

    void doDropIndexes(MongoCollection<Document> collection);

    void doDropIndexes(DropIndexOptions dropIndexOptions,MongoCollection<Document> collection);

}
