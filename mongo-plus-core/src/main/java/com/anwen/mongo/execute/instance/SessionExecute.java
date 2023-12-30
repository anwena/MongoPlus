package com.anwen.mongo.execute.instance;

import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.convert.DocumentMapperConvert;
import com.anwen.mongo.execute.Execute;
import com.anwen.mongo.model.BaseProperty;
import com.mongodb.BasicDBObject;
import com.mongodb.client.*;
import com.mongodb.client.model.CreateIndexOptions;
import com.mongodb.client.model.DropIndexOptions;
import com.mongodb.client.model.IndexModel;
import com.mongodb.client.model.IndexOptions;
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
public class SessionExecute extends Execute {

    private final ClientSession clientSession;

    public SessionExecute(MongoClient mongoClient, BaseProperty baseProperty, CollectionNameConvert collectionNameConvert, CollectionManager collectionManager,ClientSession clientSession) {
        super(mongoClient, baseProperty, collectionNameConvert, collectionManager);
        this.clientSession = clientSession;
    }

    @Override
    public InsertOneResult doSave(Document document, MongoCollection<Document> collection) {
        return collection.insertOne(clientSession,document);
    }

    @Override
    protected InsertManyResult doSaveBatch(List<Document> documentList, MongoCollection<Document> collection) {
        return collection.insertMany(clientSession,documentList);
    }

    @Override
    protected UpdateResult doUpdateById(BasicDBObject filter, BasicDBObject update, MongoCollection<Document> collection) {
        return collection.updateOne(clientSession,filter,update);
    }

    @Override
    protected UpdateResult doUpdateByColumn(Bson filter, Document document, MongoCollection<Document> collection) {
        return collection.updateMany(clientSession,filter,document);
    }

    @Override
    protected DeleteResult executeRemove(Bson filterId, MongoCollection<Document> collection) {
        return collection.deleteOne(clientSession,filterId);
    }

    @Override
    protected DeleteResult executeRemoveByColumn(Bson filter, MongoCollection<Document> collection) {
        return collection.deleteMany(clientSession,filter);
    }

    @Override
    protected DeleteResult executeRemoveBatchByIds(Bson objectIdBson, MongoCollection<Document> collection) {
        return collection.deleteMany(clientSession,objectIdBson);
    }

    @Override
    protected FindIterable<Document> doList(MongoCollection<Document> collection) {
        return collection.find(clientSession);
    }

    @Override
    protected FindIterable<Document> doList(BasicDBObject basicDBObject, BasicDBObject projectionList, BasicDBObject sortCond, MongoCollection<Document> collection) {
        return collection.find(clientSession,basicDBObject).projection(projectionList).sort(sortCond);
    }

    @Override
    protected AggregateIterable<Document> doAggregateList(List<BasicDBObject> aggregateConditionList, MongoCollection<Document> collection) {
        return collection.aggregate(clientSession,aggregateConditionList);
    }

    @Override
    protected FindIterable<Document> doGetById(BasicDBObject queryBasic, MongoCollection<Document> collection) {
        return collection.find(clientSession,queryBasic);
    }

    @Override
    protected long executeExist(BasicDBObject queryBasic, MongoCollection<Document> collection) {
        return collection.countDocuments(clientSession,queryBasic);
    }

    @Override
    protected FindIterable<Document> doGetByIds(BasicDBObject basicDBObject, MongoCollection<Document> collection) {
        return collection.find(clientSession,basicDBObject);
    }

    @Override
    protected UpdateResult executeUpdate(BasicDBObject queryBasic, BasicDBObject updateBasic, MongoCollection<Document> collection) {
        return collection.updateMany(clientSession,queryBasic,updateBasic);
    }

    @Override
    protected DeleteResult executeRemove(BasicDBObject deleteBasic, MongoCollection<Document> collection) {
        return collection.deleteMany(clientSession,deleteBasic);
    }

    @Override
    protected long executeCountByCondition(BasicDBObject basicDBObject, MongoCollection<Document> collection) {
        return collection.countDocuments(clientSession,basicDBObject);
    }

    @Override
    protected long doCount(MongoCollection<Document> collection) {
        return collection.countDocuments(clientSession);
    }

    @Override
    protected FindIterable<Document> doQueryCommand(BasicDBObject basicDBObject, MongoCollection<Document> collection) {
        return collection.find(clientSession,basicDBObject);
    }

    @Override
    protected FindIterable<Document> doGetByColumn(Bson filter, MongoCollection<Document> collection) {
        return collection.find(clientSession,filter);
    }

    @Override
    public String createIndex(Bson bson, MongoCollection<Document> collection) {
        return collection.createIndex(clientSession,bson);
    }

    @Override
    public String createIndex(Bson bson, IndexOptions indexOptions, MongoCollection<Document> collection) {
        return collection.createIndex(clientSession,bson,indexOptions);
    }

    @Override
    public List<String> createIndexes(List<IndexModel> indexes, MongoCollection<Document> collection) {
        return collection.createIndexes(clientSession,indexes);
    }

    @Override
    public List<String> createIndexes(List<IndexModel> indexes, CreateIndexOptions createIndexOptions, MongoCollection<Document> collection) {
        return collection.createIndexes(clientSession,indexes,createIndexOptions);
    }

    @Override
    public List<Document> listIndexes(MongoCollection<Document> collection) {
        return DocumentMapperConvert.indexesIterableToDocument(collection.listIndexes(clientSession));
    }

    @Override
    public void dropIndex(String indexName, MongoCollection<Document> collection) {
        collection.dropIndex(clientSession,indexName);
    }

    @Override
    public void dropIndex(String indexName, DropIndexOptions dropIndexOptions, MongoCollection<Document> collection) {
        collection.dropIndex(clientSession,indexName,dropIndexOptions);
    }

    @Override
    public void dropIndex(Bson keys, MongoCollection<Document> collection) {
        collection.dropIndex(clientSession,keys);
    }

    @Override
    public void dropIndex(Bson keys, DropIndexOptions dropIndexOptions, MongoCollection<Document> collection) {
        collection.dropIndex(clientSession,keys,dropIndexOptions);
    }

    @Override
    public void dropIndexes(MongoCollection<Document> collection) {
        collection.dropIndexes(clientSession);
    }

    @Override
    public void dropIndexes(DropIndexOptions dropIndexOptions, MongoCollection<Document> collection) {
        collection.dropIndexes(clientSession,dropIndexOptions);
    }
}
