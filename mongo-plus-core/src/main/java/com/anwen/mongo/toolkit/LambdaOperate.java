package com.anwen.mongo.toolkit;

import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.convert.Converter;
import com.anwen.mongo.convert.DocumentMapperConvert;
import com.anwen.mongo.model.BaseLambdaQueryResult;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;
import com.mongodb.BasicDBObject;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoCursor;
import org.bson.Document;

import java.util.List;
import java.util.Map;

/**
 * lambda形式调用，预构建条件
 *
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2023-12-28 17:03
 **/
public class LambdaOperate {

    public <T> List<T> getLambdaQueryResult(FindIterable<Document> iterable, Class<T> clazz) {
        return DocumentMapperConvert.mapDocumentList(iterable, clazz);
    }

    public <T> T getLambdaQueryResultOne(FindIterable<Document> iterable, Class<T> clazz) {
        try (MongoCursor<Document> cursor = iterable.iterator()) {
            if (cursor.hasNext()) {
                return DocumentMapperConvert.mapDocument(cursor.next(), clazz);
            }
            return null;
        }
    }

    public List<Map<String, Object>> getLambdaQueryResult(FindIterable<Map> iterable,Integer size) {
        return Converter.convertDocumentToMap(iterable,size);
    }

    public BaseLambdaQueryResult baseLambdaQuery(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        BasicDBObject sortCond = new BasicDBObject();
        if (CollUtil.isNotEmpty(orderList)) {
            orderList.forEach(order -> sortCond.put(order.getColumn(), order.getType()));
        }
        BasicDBObject basicDBObject = BuildCondition.buildQueryCondition(compareConditionList);
        if (CollUtil.isNotEmpty(basicDBObjectList)){
            basicDBObjectList.forEach(basic -> {
                basicDBObject.putAll(basic.toMap());
            });
        }
        return new BaseLambdaQueryResult(basicDBObject,BuildCondition.buildProjection(projectionList),sortCond);
    }

    public <T> PageResult<T> getLambdaQueryResultPage(FindIterable<Document> documentFindIterable,long totalSize, PageParam pageParams,Class<T> clazz) {
        PageResult<T> pageResult = new PageResult<>();
        pageResult.setPageNum(pageParams.getPageNum());
        pageResult.setPageSize(pageParams.getPageSize());
        pageResult.setContentData(DocumentMapperConvert.mapDocumentList(documentFindIterable.skip((pageParams.getPageNum() - 1) * pageParams.getPageSize()).limit(pageParams.getPageSize()), clazz));
        // 不查询总条数，总条数=当前页的总数
        if (totalSize == -1) {
            totalSize = pageResult.getContentData().size();
        }
        pageResult.setTotalSize(totalSize);
        pageResult.setTotalPages((totalSize + pageParams.getPageSize() - 1) / pageParams.getPageSize());
        return pageResult;
    }

    public PageResult<Map<String, Object>> getLambdaQueryResultPage(FindIterable<Map> documentFindIterable,long totalSize, PageParam pageParams) {
        PageResult<Map<String, Object>> pageResult = new PageResult<>();
        pageResult.setPageNum(pageParams.getPageNum());
        pageResult.setPageSize(pageParams.getPageSize());
        pageResult.setTotalSize(totalSize);
        pageResult.setTotalPages((totalSize + pageParams.getPageSize() - 1) / pageParams.getPageSize());
        pageResult.setContentData(Converter.convertDocumentToMap(documentFindIterable.skip((pageParams.getPageNum() - 1) * pageParams.getPageSize()).limit(pageParams.getPageSize())));
        return pageResult;
    }

//    public PageResult<Map<String, Object>> getLambdaQueryResultPage(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, PageParam pageParams) {
//        return getLambdaQueryResultPage(MongoTransactionContext.getClientSessionContext(),collectionName,compareConditionList,orderList,projectionList,basicDBObjectList,pageParams);
//    }
//
//    public PageResult<Map<String, Object>> getLambdaQueryResultPage(ClientSession clientSession,String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, PageParam pageParams) {
//        PageResult<Map<String, Object>> pageResult = new PageResult<>();
//        FindIterable<Map> documentFindIterable = baseLambdaQuery(clientSession,collectionName, compareConditionList, orderList,projectionList,basicDBObjectList);
//        long totalSize = doCount(collectionName,compareConditionList);
//        pageResult.setPageNum(pageParams.getPageNum());
//        pageResult.setPageSize(pageParams.getPageSize());
//        pageResult.setTotalSize(totalSize);
//        pageResult.setTotalPages((totalSize + pageParams.getPageSize() - 1) / pageParams.getPageSize());
//        pageResult.setContentData(Converter.convertDocumentToMap(documentFindIterable.skip((pageParams.getPageNum() - 1) * pageParams.getPageSize()).limit(pageParams.getPageSize())));
//        return pageResult;
//    }

}
