package com.anwen.mongo.interceptor.business;

import com.anwen.mongo.aggregate.AggregateWrapper;
import com.anwen.mongo.cache.codec.MapCodecCache;
import com.anwen.mongo.cache.global.DataSourceNameCache;
import com.anwen.mongo.cache.global.TenantCache;
import com.anwen.mongo.conditions.query.QueryWrapper;
import com.anwen.mongo.enums.AggregateEnum;
import com.anwen.mongo.handlers.TenantHandler;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.model.AggregateBasicDBObject;
import com.anwen.mongo.model.MutablePair;
import com.anwen.mongo.model.QueryParam;
import com.anwen.mongo.toolkit.BsonUtil;
import com.anwen.mongo.toolkit.CollUtil;
import com.anwen.mongo.toolkit.Filters;
import com.mongodb.BasicDBObject;
import com.mongodb.MongoNamespace;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.CountOptions;
import com.mongodb.client.model.InsertOneModel;
import com.mongodb.client.model.UpdateManyModel;
import com.mongodb.client.model.WriteModel;
import org.bson.BSONObject;
import org.bson.BsonDocument;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 多租户拦截器
 *
 * @author anwen
 * @date 2024/6/27 上午10:56
 */
public class TenantInterceptor implements Interceptor {

    private final TenantHandler tenantHandler;

    public TenantInterceptor(TenantHandler tenantHandler) {
        this.tenantHandler = tenantHandler;
    }

    @Override
    public int order() {
        return 0;
    }

    @Override
    public List<Document> executeSave(List<Document> documentList, MongoCollection<Document> collection) {
        if (checkTenant(collection) || tenantHandler.ignoreInsert(new ArrayList<>(documentList.get(0).keySet()),tenantHandler.getTenantIdColumn())){
            return documentList;
        }
        if (CollUtil.isNotEmpty(documentList)) {
            documentList.forEach(document -> {
                if (!document.containsKey(tenantHandler.getTenantIdColumn())) {
                    document.put(tenantHandler.getTenantIdColumn(), tenantHandler.getTenantId());
                }
            });
        }
        return documentList;
    }

    @Override
    public Bson executeRemove(Bson filter, MongoCollection<Document> collection) {
        if (!checkTenant(collection)){
            if (filter == null){
                filter = Filters.eq(tenantHandler.getTenantIdColumn(),tenantHandler.getTenantId());
            } else if (!filter.toBsonDocument(BsonDocument.class, MapCodecCache.getDefaultCodecRegistry()).containsKey(tenantHandler.getTenantIdColumn())){
                filter = BsonUtil.addToMap(filter,tenantHandler.getTenantIdColumn(),tenantHandler.getTenantId());
            }
        }
        return filter;
    }

    @Override
    public MutablePair<Bson, Bson> executeUpdate(Bson queryBasic, Bson updateBasic, MongoCollection<Document> collection) {
        if (!checkTenant(collection)){
            if (queryBasic == null){
                queryBasic = Filters.eq(tenantHandler.getTenantIdColumn(),tenantHandler.getTenantId());
            } else if (!queryBasic.toBsonDocument(BsonDocument.class, MapCodecCache.getDefaultCodecRegistry()).containsKey(tenantHandler.getTenantIdColumn())) {
                queryBasic = BsonUtil.addToMap(queryBasic, tenantHandler.getTenantIdColumn(), tenantHandler.getTenantId());
            }
        }
        return new MutablePair<>(queryBasic,updateBasic);
    }

    @Override
    public List<MutablePair<Bson, Bson>> executeUpdate(List<MutablePair<Bson, Bson>> updatePairList, MongoCollection<Document> collection) {
        if (!checkTenant(collection)){
            for (MutablePair<Bson, Bson> mutablePair : updatePairList) {
                Bson queryBasic = mutablePair.getLeft();
                if (queryBasic == null) {
                    queryBasic = Filters.eq(tenantHandler.getTenantIdColumn(), tenantHandler.getTenantId());
                } else if (!queryBasic.toBsonDocument(BsonDocument.class, MapCodecCache.getDefaultCodecRegistry()).containsKey(tenantHandler.getTenantIdColumn())) {
                    queryBasic = BsonUtil.addToMap(queryBasic, tenantHandler.getTenantIdColumn(), tenantHandler.getTenantId());
                }
                mutablePair.setLeft(queryBasic);
            }
        }
        return updatePairList;
    }

    @Override
    public QueryParam executeQuery(Bson queryBasic, BasicDBObject projectionList, BasicDBObject sortCond, MongoCollection<Document> collection) {
        if (!checkTenant(collection)){
            if (queryBasic == null){
                queryBasic = Filters.eq(tenantHandler.getTenantIdColumn(),tenantHandler.getTenantId());
            } else if (!queryBasic.toBsonDocument(BsonDocument.class, MapCodecCache.getDefaultCodecRegistry()).containsKey(tenantHandler.getTenantIdColumn())) {
                queryBasic = BsonUtil.addToMap(queryBasic, tenantHandler.getTenantIdColumn(), tenantHandler.getTenantId());
            }
        }
        return new QueryParam(queryBasic,projectionList,sortCond);
    }

    @Override
    public List<AggregateBasicDBObject> executeAggregate(List<AggregateBasicDBObject> aggregateConditionList, MongoCollection<Document> collection) {
        if (!checkTenant(collection)){
            Bson matchBson = new AggregateWrapper().match(new QueryWrapper<>().eq(tenantHandler.getTenantIdColumn(), tenantHandler.getTenantId())).getAggregateConditionList().get(0);
            if (CollUtil.isNotEmpty(aggregateConditionList)) {
                aggregateConditionList.forEach(aggregateBasicDBObject -> {
                    if (aggregateBasicDBObject.containsKey(AggregateEnum.MATCH.getValue())) {
                        Bson bson = (Bson) aggregateBasicDBObject.get(AggregateEnum.MATCH.getValue());
                        if (!bson.toBsonDocument(BsonDocument.class, MapCodecCache.getDefaultCodecRegistry()).containsKey(tenantHandler.getTenantIdColumn())) {
                            BsonUtil.addToMap(bson, tenantHandler.getTenantIdColumn(), tenantHandler.getTenantId());
                        }
                    } else {
                        aggregateConditionList.add(new AggregateBasicDBObject(BasicDBObject.parse(matchBson.toBsonDocument(BsonDocument.class, MapCodecCache.getDefaultCodecRegistry()).toJson()), 0));
                    }
                });
            } else {
                aggregateConditionList.add(new AggregateBasicDBObject(BasicDBObject.parse(matchBson.toBsonDocument(BsonDocument.class, MapCodecCache.getDefaultCodecRegistry()).toJson()), 0));
            }
        }
        return aggregateConditionList;
    }

    @Override
    public List<Bson> executeAggregates(List<Bson> aggregateConditionList, MongoCollection<Document> collection) {
        if (!checkTenant(collection)) {
            Bson matchBson = new AggregateWrapper()
                    .match(new QueryWrapper<>().eq(tenantHandler.getTenantIdColumn(), tenantHandler.getTenantId()))
                    .getAggregateConditionList()
                    .get(0);

            boolean hasMatch = aggregateConditionList.stream()
                    .anyMatch(bson -> bson.toBsonDocument(BsonDocument.class, MapCodecCache.getDefaultCodecRegistry())
                            .containsKey(AggregateEnum.MATCH.getValue()));

            if (hasMatch) {
                for (int i = 0; i < aggregateConditionList.size(); i++) {
                    Bson bson = aggregateConditionList.get(i);
                    BsonDocument bsonDocument = bson.toBsonDocument(BsonDocument.class, MapCodecCache.getDefaultCodecRegistry());

                    if (bsonDocument.containsKey(AggregateEnum.MATCH.getValue())) {
                        BsonDocument matchBsonDocument = bsonDocument.get(AggregateEnum.MATCH.getValue()).asDocument();

                        if (!matchBsonDocument.containsKey(tenantHandler.getTenantIdColumn())) {
                            matchBsonDocument.put(tenantHandler.getTenantIdColumn(), tenantHandler.getTenantId());
                            aggregateConditionList.set(i, bsonDocument);
                        }
                    }
                }
            } else {
                aggregateConditionList.add(0, matchBson);
            }
        }
        return aggregateConditionList;
    }

    @Override
    public MutablePair<BasicDBObject, CountOptions> executeCount(BasicDBObject queryBasic, CountOptions countOptions, MongoCollection<Document> collection) {
        if (!checkTenant(collection)){
            if (queryBasic == null){
                queryBasic = new BasicDBObject(Filters.eq(tenantHandler.getTenantIdColumn(),tenantHandler.getTenantId()).toBsonDocument(BsonDocument.class, MapCodecCache.getDefaultCodecRegistry()));
            } else if (!queryBasic.toBsonDocument(BsonDocument.class, MapCodecCache.getDefaultCodecRegistry()).containsKey(tenantHandler.getTenantIdColumn())) {
                BsonUtil.addToMap(queryBasic, tenantHandler.getTenantIdColumn(), tenantHandler.getTenantId());
            }
        }
        return new MutablePair<>(queryBasic,countOptions);
    }

    @Override
    public List<WriteModel<Document>> executeBulkWrite(List<WriteModel<Document>> writeModelList, MongoCollection<Document> collection) {
        if (!checkTenant(collection) && CollUtil.isNotEmpty(writeModelList)){
            List<Document> insertDocumentList = writeModelList.stream().filter(writeModel -> writeModel instanceof InsertOneModel).collect(Collectors.toList()).stream().map(writeModel -> ((InsertOneModel<Document>) writeModel).getDocument()).collect(Collectors.toList());
            if (CollUtil.isNotEmpty(insertDocumentList)) {
                executeSave(insertDocumentList, collection);
            }
            List<WriteModel<Document>> updateDocumentList = writeModelList.stream().filter(writeModel -> writeModel instanceof UpdateManyModel).collect(Collectors.toList());
            if (CollUtil.isNotEmpty(updateDocumentList)) {
                updateDocumentList.forEach(writeModel -> {
                    UpdateManyModel<Document> updateManyModel = (UpdateManyModel<Document>) writeModel;
                    executeUpdate(updateManyModel.getFilter(), updateManyModel.getUpdate(), collection);
                });
            }
        }
        return writeModelList;
    }

    /**
     * 校验租户
     * @param collection 集合
     * @return {@link boolean}
     * @author anwen
     * @date 2024/6/27 上午11:07
     */
    private boolean checkTenant(MongoCollection<Document> collection){
        MongoNamespace namespace = collection.getNamespace();
        String collectionName = namespace.getCollectionName();
        String databaseName = namespace.getDatabaseName();
        String dataSource = DataSourceNameCache.getDataSource();
        Boolean ignoreTenant = TenantCache.getIgnoreTenant();
        if (ignoreTenant != null){
            return ignoreTenant;
        }
        return tenantHandler.ignoreCollection(collectionName) == tenantHandler.ignoreDatabase(databaseName) == tenantHandler.ignoreDataSource(dataSource);
    }

}
