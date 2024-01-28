package com.anwen.mongo.execute;

import com.anwen.mongo.model.AggregateBasicDBObject;
import com.mongodb.BasicDBObject;
import com.mongodb.bulk.BulkWriteResult;
import com.mongodb.client.AggregateIterable;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.*;
import com.mongodb.client.result.DeleteResult;
import com.mongodb.client.result.InsertManyResult;
import com.mongodb.client.result.InsertOneResult;
import com.mongodb.client.result.UpdateResult;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.util.List;

/**
 * 抽象的执行器
 *
 * @author JiaChaoYang
 **/
public interface Execute {

    InsertOneResult doSave(Document document, MongoCollection<Document> collection);

    InsertManyResult doSaveBatch(List<Document> documentList, MongoCollection<Document> collection);

    BulkWriteResult bulkWrite(List<WriteModel<Document>> writeModelList, MongoCollection<Document> collection);

    DeleteResult executeRemove(Bson filter, MongoCollection<Document> collection);

    FindIterable<Document> doList(MongoCollection<Document> collection);

    <T> FindIterable<T> doList(MongoCollection<Document> collection,Class<T> clazz);

    FindIterable<Document> doList(BasicDBObject basicDBObject, BasicDBObject projectionList, BasicDBObject sortCond, MongoCollection<Document> collection);

    <T> FindIterable<T> doList(BasicDBObject basicDBObject, BasicDBObject projectionList, BasicDBObject sortCond, MongoCollection<Document> collection,Class<T> clazz);

    AggregateIterable<Document> doAggregateList(List<AggregateBasicDBObject> aggregateConditionList,MongoCollection<Document> collection);

    <T> AggregateIterable<T> doAggregateList(List<AggregateBasicDBObject> aggregateConditionList,MongoCollection<Document> collection,Class<T> clazz);

    FindIterable<Document> doGetById(BasicDBObject queryBasic,MongoCollection<Document> collection);

    <T> FindIterable<T> doGetByIds(BasicDBObject queryBasic, MongoCollection<Document> collection, Class<T> clazz);

    long executeExist(BasicDBObject queryBasic, MongoCollection<Document> collection);

    UpdateResult executeUpdate(Bson queryBasic,Bson updateBasic,MongoCollection<Document> collection);

    DeleteResult executeRemove(BasicDBObject deleteBasic,MongoCollection<Document> collection);

    long executeCountByCondition(BasicDBObject basicDBObject,MongoCollection<Document> collection);

    long executeCountByCondition(BasicDBObject basicDBObject,MongoCollection<Document> collection, CountOptions var2);

    long doCount(MongoCollection<Document> collection);

    FindIterable<Document> doQueryCommand(BasicDBObject basicDBObject,MongoCollection<Document> collection);

    <T> FindIterable<T> doQueryCommand(BasicDBObject basicDBObject,MongoCollection<Document> collection,Class<T> clazz);

    FindIterable<Document> doGetByColumn(Bson filter,MongoCollection<Document> collection);

    <T> FindIterable<T> doGetByColumn(Bson filter,MongoCollection<Document> collection,Class<T> clazz);

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
