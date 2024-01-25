package com.anwen.mongo.execute.instance;

import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.convert.DocumentMapperConvert;
import com.anwen.mongo.execute.AbstractExecute;
import com.anwen.mongo.model.AggregateBasicDBObject;
import com.mongodb.BasicDBObject;
import com.mongodb.client.AggregateIterable;
import com.mongodb.client.ClientSession;
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
 * session实例
 *
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2023-12-28 11:03
 **/
public class SessionExecute extends AbstractExecute {

    private final ClientSession clientSession;

    public SessionExecute(CollectionNameConvert collectionNameConvert, CollectionManager collectionManager, ClientSession clientSession) {
        super(collectionNameConvert, collectionManager);
        this.clientSession = clientSession;
    }

    @Override
    public InsertOneResult doSave(Document document, MongoCollection<Document> collection) {
        return collection.insertOne(clientSession,document);
    }

    @Override
    public InsertManyResult doSaveBatch(List<Document> documentList, MongoCollection<Document> collection) {
        return collection.insertMany(clientSession,documentList);
    }

    @Override
    public DeleteResult executeRemove(Bson filter, MongoCollection<Document> collection) {
        return collection.deleteOne(clientSession, filter);
    }

    @Override
    public FindIterable<Document> doList(MongoCollection<Document> collection) {
        return collection.find(clientSession);
    }

    @Override
    public <T> FindIterable<T> doList(MongoCollection<Document> collection, Class<T> clazz) {
        return collection.find(clientSession,clazz);
    }

    @Override
    public FindIterable<Document> doList(BasicDBObject basicDBObject, BasicDBObject projectionList, BasicDBObject sortCond, MongoCollection<Document> collection) {
        return collection.find(clientSession,basicDBObject).projection(projectionList).sort(sortCond);
    }

    @Override
    public <T> FindIterable<T> doList(BasicDBObject basicDBObject, BasicDBObject projectionList, BasicDBObject sortCond, MongoCollection<Document> collection, Class<T> clazz) {
        return collection.find(clientSession,basicDBObject,clazz).projection(projectionList).sort(sortCond);
    }

    @Override
    public AggregateIterable<Document> doAggregateList(List<AggregateBasicDBObject> aggregateConditionList, MongoCollection<Document> collection) {
        return collection.aggregate(clientSession,aggregateConditionList);
    }

    @Override
    public <T> AggregateIterable<T> doAggregateList(List<AggregateBasicDBObject> aggregateConditionList, MongoCollection<Document> collection, Class<T> clazz) {
        return collection.aggregate(clientSession,aggregateConditionList, clazz);
    }
    @Override
    public FindIterable<Document> doGetById(BasicDBObject queryBasic, MongoCollection<Document> collection) {
        return collection.find(clientSession,queryBasic);
    }

    @Override
    public <T> FindIterable<T> doGetByIds(BasicDBObject queryBasic, MongoCollection<Document> collection, Class<T> clazz) {
        return collection.find(queryBasic,clazz);
    }

    @Override
    public long executeExist(BasicDBObject queryBasic, MongoCollection<Document> collection) {
        return collection.countDocuments(clientSession,queryBasic);
    }

    @Override
    public FindIterable<Document> doGetByIds(BasicDBObject basicDBObject, MongoCollection<Document> collection) {
        return collection.find(clientSession,basicDBObject);
    }

    @Override
    public UpdateResult executeUpdate(Bson queryBasic, Bson updateBasic, MongoCollection<Document> collection) {
        return collection.updateMany(clientSession,queryBasic,updateBasic);
    }

    @Override
    public DeleteResult executeRemove(BasicDBObject deleteBasic, MongoCollection<Document> collection) {
        return collection.deleteMany(clientSession,deleteBasic);
    }

    @Override
    public long executeCountByCondition(BasicDBObject basicDBObject, MongoCollection<Document> collection) {
        return collection.countDocuments(clientSession,basicDBObject);
    }

    @Override
    public long executeCountByCondition(BasicDBObject basicDBObject, MongoCollection<Document> collection, CountOptions var2) {
        return collection.countDocuments(clientSession, basicDBObject, var2);
    }

    @Override
    public long doCount(MongoCollection<Document> collection) {
        return collection.countDocuments(clientSession);
    }

    @Override
    public FindIterable<Document> doQueryCommand(BasicDBObject basicDBObject, MongoCollection<Document> collection) {
        return collection.find(clientSession,basicDBObject);
    }

    @Override
    public <T> FindIterable<T> doQueryCommand(BasicDBObject basicDBObject, MongoCollection<Document> collection, Class<T> clazz) {
        return collection.find(clientSession,basicDBObject,clazz);
    }

    @Override
    public FindIterable<Document> doGetByColumn(Bson filter, MongoCollection<Document> collection) {
        return collection.find(clientSession,filter);
    }

    @Override
    public <T> FindIterable<T> doGetByColumn(Bson filter, MongoCollection<Document> collection, Class<T> clazz) {
        return collection.find(clientSession,filter,clazz);
    }

    @Override
    public String doCreateIndex(Bson bson, MongoCollection<Document> collection) {
        return collection.createIndex(clientSession,bson);
    }

    @Override
    public String doCreateIndex(Bson bson, IndexOptions indexOptions, MongoCollection<Document> collection) {
        return collection.createIndex(clientSession,bson,indexOptions);
    }

    @Override
    public List<String> doCreateIndexes(List<IndexModel> indexes, MongoCollection<Document> collection) {
        return collection.createIndexes(clientSession,indexes);
    }

    @Override
    public List<String> doCreateIndexes(List<IndexModel> indexes, CreateIndexOptions createIndexOptions, MongoCollection<Document> collection) {
        return collection.createIndexes(clientSession,indexes,createIndexOptions);
    }

    @Override
    public List<Document> doListIndexes(MongoCollection<Document> collection) {
        return DocumentMapperConvert.indexesIterableToDocument(collection.listIndexes(clientSession));
    }

    @Override
    public void doDropIndex(String indexName, MongoCollection<Document> collection) {
        collection.dropIndex(clientSession,indexName);
    }

    @Override
    public void doDropIndex(String indexName, DropIndexOptions dropIndexOptions, MongoCollection<Document> collection) {
        collection.dropIndex(clientSession,indexName,dropIndexOptions);
    }

    @Override
    public void doDropIndex(Bson keys, MongoCollection<Document> collection) {
        collection.dropIndex(clientSession,keys);
    }

    @Override
    public void doDropIndex(Bson keys, DropIndexOptions dropIndexOptions, MongoCollection<Document> collection) {
        collection.dropIndex(clientSession,keys,dropIndexOptions);
    }

    @Override
    public void doDropIndexes(MongoCollection<Document> collection) {
        collection.dropIndexes(clientSession);
    }

    @Override
    public void doDropIndexes(DropIndexOptions dropIndexOptions, MongoCollection<Document> collection) {
        collection.dropIndexes(clientSession,dropIndexOptions);
    }
}
