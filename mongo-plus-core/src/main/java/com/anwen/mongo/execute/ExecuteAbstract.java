package com.anwen.mongo.execute;

import com.mongodb.BasicDBObject;
import com.mongodb.client.AggregateIterable;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoCollection;
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
 * 抽象的执行器
 *
 * @author JiaChaoYang
 **/
public abstract class ExecuteAbstract {

    protected abstract InsertOneResult doSave(Document document, MongoCollection<Document> collection);

    protected abstract InsertManyResult doSaveBatch(List<Document> documentList, MongoCollection<Document> collection);

    protected abstract UpdateResult doUpdateById(BasicDBObject filter, BasicDBObject update, MongoCollection<Document> collection);

    protected abstract UpdateResult doUpdateByColumn(Bson filter, Document document, MongoCollection<Document> collection);

    protected abstract DeleteResult executeRemove(Bson filterId, MongoCollection<Document> collection);

    protected abstract DeleteResult executeRemoveByColumn(Bson filter,MongoCollection<Document> collection);

    protected abstract DeleteResult executeRemoveBatchByIds(Bson objectIdBson,MongoCollection<Document> collection);

    protected abstract FindIterable<Document> doList(MongoCollection<Document> collection);

    protected abstract FindIterable<Document> doList(BasicDBObject basicDBObject, BasicDBObject projectionList, BasicDBObject sortCond, MongoCollection<Document> collection);

    protected abstract AggregateIterable<Document> doAggregateList(List<BasicDBObject> aggregateConditionList,MongoCollection<Document> collection);

    protected abstract FindIterable<Document> doGetById(BasicDBObject queryBasic,MongoCollection<Document> collection);

    protected abstract long executeExist(BasicDBObject queryBasic, MongoCollection<Document> collection);

    protected abstract FindIterable<Document> doGetByIds(BasicDBObject basicDBObject,MongoCollection<Document> collection);

    protected abstract UpdateResult executeUpdate(BasicDBObject queryBasic,BasicDBObject updateBasic,MongoCollection<Document> collection);

    protected abstract DeleteResult executeRemove(BasicDBObject deleteBasic,MongoCollection<Document> collection);

    protected abstract long executeCountByCondition(BasicDBObject basicDBObject,MongoCollection<Document> collection);

    protected abstract long doCount(MongoCollection<Document> collection);

    protected abstract FindIterable<Document> doQueryCommand(BasicDBObject basicDBObject,MongoCollection<Document> collection);

    protected abstract FindIterable<Document> doGetByColumn(Bson filter,MongoCollection<Document> collection);



    public abstract String createIndex(Bson bson,MongoCollection<Document> collection);

    public abstract String createIndex(Bson bson, IndexOptions indexOptions, MongoCollection<Document> collection);

    public abstract List<String> createIndexes(List<IndexModel> indexes, MongoCollection<Document> collection);

    public abstract List<String> createIndexes(List<IndexModel> indexes, CreateIndexOptions createIndexOptions, MongoCollection<Document> collection);

    public abstract List<Document> listIndexes(MongoCollection<Document> collection);

    public abstract void dropIndex(String indexName,MongoCollection<Document> collection);

    public abstract void dropIndex(String indexName, DropIndexOptions dropIndexOptions, MongoCollection<Document> collection);

    public abstract void dropIndex(Bson keys,MongoCollection<Document> collection);

    public abstract void dropIndex(Bson keys,DropIndexOptions dropIndexOptions,MongoCollection<Document> collection);

    public abstract void dropIndexes(MongoCollection<Document> collection);

    public abstract void dropIndexes(DropIndexOptions dropIndexOptions,MongoCollection<Document> collection);

}
