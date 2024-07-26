package com.anwen.mongo.toolkit;

import com.anwen.mongo.cache.codec.MapCodecCache;
import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.convert.Converter;
import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.mapping.TypeReference;
import com.anwen.mongo.model.BaseLambdaQueryResult;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;
import com.mongodb.BasicDBObject;
import com.mongodb.client.FindIterable;
import org.bson.BsonDocument;
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

    public BaseLambdaQueryResult baseLambdaQuery(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        BasicDBObject sortCond = new BasicDBObject();
        if (CollUtil.isNotEmpty(orderList)) {
            orderList.forEach(order -> sortCond.put(order.getColumn(), order.getType()));
        }
        BasicDBObject basicDBObject = BuildCondition.buildQueryCondition(compareConditionList);
        if (CollUtil.isNotEmpty(basicDBObjectList)){
            basicDBObjectList.forEach(basic -> {
                basicDBObject.putAll(basic.toBsonDocument(BsonDocument.class, MapCodecCache.getDefaultCodecRegistry()));
            });
        }
        return new BaseLambdaQueryResult(basicDBObject,BuildCondition.buildProjection(projectionList),sortCond);
    }

    public <T> PageResult<T> getLambdaQueryResultPage(FindIterable<Document> documentFindIterable, long totalSize, PageParam pageParams, TypeReference<T> typeReference, MongoConverter mongoConverter) {
        PageResult<T> pageResult = new PageResult<>();
        pageResult.setPageNum(pageParams.getPageNum());
        pageResult.setPageSize(pageParams.getPageSize());
        pageResult.setContentData(mongoConverter.read(documentFindIterable.skip((pageParams.getPageNum() - 1) * pageParams.getPageSize()).limit(pageParams.getPageSize()), typeReference));
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

}
