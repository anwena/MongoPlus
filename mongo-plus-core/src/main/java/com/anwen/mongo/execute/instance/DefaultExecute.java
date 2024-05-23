package com.anwen.mongo.execute.instance;

import com.anwen.mongo.convert.DocumentMapperConvert;
import com.anwen.mongo.execute.Execute;
import com.anwen.mongo.model.AggregateBasicDBObject;
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
import java.util.Optional;

/**
 * 默认执行器实例
 *
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2023-12-28 11:03
 **/
public class DefaultExecute implements Execute {

    @Override
    public InsertManyResult executeSave(List<Document> documentList, MongoCollection<Document> collection) {
        return collection.insertMany(documentList);
    }

    @Override
    public BulkWriteResult executeBulkWrite(List<WriteModel<Document>> writeModelList, MongoCollection<Document> collection) {
        return collection.bulkWrite(writeModelList);
    }

    @Override
    public DeleteResult executeRemove(Bson filter, MongoCollection<Document> collection) {
        return collection.deleteMany(filter);
    }

    @Override
    public <T> FindIterable<T> executeQuery(Bson queryBasic, BasicDBObject projectionList, BasicDBObject sortCond, MongoCollection<Document> collection, Class<T> clazz) {
        return Optional.ofNullable(queryBasic)
                .map(qb -> collection.find(qb,clazz))
                .orElseGet(() -> collection.find(clazz))
                .projection(projectionList)
                .sort(sortCond);
    }

    @Override
    public <T> AggregateIterable<T> executeAggregate(List<AggregateBasicDBObject> aggregateConditionList, MongoCollection<Document> collection, Class<T> clazz) {
        return collection.aggregate(aggregateConditionList, clazz);
    }

    @Override
    public long executeCount(BasicDBObject queryBasic, CountOptions countOptions, MongoCollection<Document> collection) {
        return Optional.ofNullable(countOptions).map(co -> collection.countDocuments(queryBasic,co)).orElseGet(() -> collection.countDocuments(queryBasic));
    }

    @Override
    public long estimatedDocumentCount(MongoCollection<Document> collection) {
        return collection.estimatedDocumentCount();
    }

    @Override
    public UpdateResult executeUpdate(Bson queryBasic, Bson updateBasic, MongoCollection<Document> collection) {
        return collection.updateMany(queryBasic,updateBasic);
    }

    @Override
    public String doCreateIndex(Bson bson, MongoCollection<Document> collection) {
        return collection.createIndex(bson);
    }

    @Override
    public String doCreateIndex(Bson bson, IndexOptions indexOptions, MongoCollection<Document> collection) {
        return collection.createIndex(bson,indexOptions);
    }

    @Override
    public List<String> doCreateIndexes(List<IndexModel> indexes, MongoCollection<Document> collection) {
        return collection.createIndexes(indexes);
    }

    @Override
    public List<String> doCreateIndexes(List<IndexModel> indexes, CreateIndexOptions createIndexOptions, MongoCollection<Document> collection) {
        return collection.createIndexes(indexes,createIndexOptions);
    }

    @Override
    public List<Document> doListIndexes(MongoCollection<Document> collection) {
        return DocumentMapperConvert.indexesIterableToDocument(collection.listIndexes());
    }

    @Override
    public void doDropIndex(String indexName, MongoCollection<Document> collection) {
        collection.dropIndex(indexName);
    }

    @Override
    public void doDropIndex(String indexName, DropIndexOptions dropIndexOptions, MongoCollection<Document> collection) {
        collection.dropIndex(indexName,dropIndexOptions);
    }

    @Override
    public void doDropIndex(Bson keys, MongoCollection<Document> collection) {
        collection.dropIndex(keys);
    }

    @Override
    public void doDropIndex(Bson keys, DropIndexOptions dropIndexOptions, MongoCollection<Document> collection) {
        collection.dropIndex(keys,dropIndexOptions);
    }

    @Override
    public void doDropIndexes(MongoCollection<Document> collection) {
        collection.dropIndexes();
    }

    @Override
    public void doDropIndexes(DropIndexOptions dropIndexOptions, MongoCollection<Document> collection) {
        collection.dropIndexes(dropIndexOptions);
    }
}
