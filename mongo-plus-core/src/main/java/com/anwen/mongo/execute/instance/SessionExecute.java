package com.anwen.mongo.execute.instance;

import com.anwen.mongo.convert.DocumentMapperConvert;
import com.anwen.mongo.execute.Execute;
import com.anwen.mongo.model.AggregateBasicDBObject;
import com.anwen.mongo.model.MutablePair;
import com.mongodb.BasicDBObject;
import com.mongodb.bulk.BulkWriteResult;
import com.mongodb.client.AggregateIterable;
import com.mongodb.client.ClientSession;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.*;
import com.mongodb.client.result.DeleteResult;
import com.mongodb.client.result.InsertManyResult;
import com.mongodb.client.result.UpdateResult;
import org.bson.BsonValue;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

/**
 * session实例
 *
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2023-12-28 11:03
 **/
public class SessionExecute implements Execute {

    private final ClientSession clientSession;

    public SessionExecute(ClientSession clientSession) {
        this.clientSession = clientSession;
    }

    @Override
    public InsertManyResult executeSave(List<Document> documentList, MongoCollection<Document> collection) {
        return collection.insertMany(clientSession,documentList);
    }

    @Override
    public BulkWriteResult executeBulkWrite(List<WriteModel<Document>> writeModelList, MongoCollection<Document> collection) {
        return collection.bulkWrite(clientSession,writeModelList);
    }

    @Override
    public DeleteResult executeRemove(Bson filter, MongoCollection<Document> collection) {
        return collection.deleteMany(clientSession, filter);
    }

    @Override
    public <T> FindIterable<T> executeQuery(Bson queryBasic, BasicDBObject projectionList, BasicDBObject sortCond, MongoCollection<Document> collection, Class<T> clazz) {
        return Optional.ofNullable(queryBasic)
                .map(qb -> collection.find(clientSession,qb,clazz))
                .orElseGet(() -> collection.find(clientSession,clazz))
                .projection(projectionList)
                .sort(sortCond);
    }

    @Override
    public <T> AggregateIterable<T> executeAggregateOld(List<AggregateBasicDBObject> aggregateConditionList, MongoCollection<Document> collection, Class<T> clazz) {
        return collection.aggregate(clientSession,aggregateConditionList, clazz);
    }

    @Override
    public <T> AggregateIterable<T> executeAggregate(List<? extends Bson> aggregateConditionList, MongoCollection<Document> collection, Class<T> clazz) {
        return collection.aggregate(clientSession,aggregateConditionList,clazz);
    }

    @Override
    public long executeCount(BasicDBObject queryBasic, CountOptions countOptions, MongoCollection<Document> collection) {
        return Optional.ofNullable(countOptions).map(co -> collection.countDocuments(clientSession, queryBasic,co)).orElseGet(() -> collection.countDocuments(clientSession, queryBasic));
    }

    @Override
    public long estimatedDocumentCount(MongoCollection<Document> collection) {
        return collection.countDocuments();
    }

    @Override
    public UpdateResult executeUpdate(Bson queryBasic, Bson updateBasic, MongoCollection<Document> collection) {
        return collection.updateMany(clientSession,queryBasic,updateBasic);
    }

    @Override
    public UpdateResult executeUpdate(List<MutablePair<Bson, Bson>> bsonPairList, MongoCollection<Document> collection) {
        AtomicReference<Long> matchedCount = new AtomicReference<>(0L);
        AtomicReference<Long> modifiedCount = new AtomicReference<>(0L);
        AtomicReference<BsonValue> upstartedId = new AtomicReference<>();
        bsonPairList.forEach(bsonPair -> {
            UpdateResult updateResult = collection.updateMany(clientSession,bsonPair.getLeft(), bsonPair.getRight());
            matchedCount.updateAndGet(v -> v + updateResult.getMatchedCount());
            modifiedCount.updateAndGet(v -> v + updateResult.getModifiedCount());
            upstartedId.set(updateResult.getUpsertedId());
        });
        return UpdateResult.acknowledged(matchedCount.get(), modifiedCount.get(), upstartedId.get());
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
