package com.anwen.mongo.interceptor;

import com.anwen.mongo.model.AggregateBasicDBObject;
import com.anwen.mongo.model.MutablePair;
import com.anwen.mongo.model.QueryParam;
import com.mongodb.BasicDBObject;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.CountOptions;
import com.mongodb.client.model.WriteModel;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.util.List;

/**
 * 拦截器，代理{@link com.anwen.mongo.execute.Execute}接口，增删改查会经过
 *
 * @author JiaChaoYang
 **/
public interface Interceptor {

    /**
     * 拦截器 排序
     *
     * @return 升序 从小到大
     */
    default int order() {
        return Integer.MAX_VALUE;
    }

    /**
     * 添加拦截方法
     *
     * @param documentList 经过添加方法的值
     * @return java.util.List<org.bson.Document>
     * @author JiaChaoYang
     * @date 2024/3/17 0:37
     */
    default List<Document> executeSave(List<Document> documentList) {
        return documentList;
    }

    /**
     * 删除拦截方法
     *
     * @author JiaChaoYang
     * @date 2024/3/19 19:18
     */
    default Bson executeRemove(Bson filter) {
        return filter;
    }

    /**
     * 修改拦截方法
     *
     * @author JiaChaoYang
     * @date 2024/3/19 19:18
     */
    default MutablePair<Bson, Bson> executeUpdate(Bson queryBasic, Bson updateBasic) {
        return new MutablePair<>(queryBasic, updateBasic);
    }

    /**
     * 查询拦截方法
     *
     * @author JiaChaoYang
     * @date 2024/3/19 19:18
     */
    default QueryParam executeQuery(Bson queryBasic, BasicDBObject projectionList, BasicDBObject sortCond) {
        return new QueryParam(queryBasic, projectionList, sortCond);
    }

    /**
     * 管道拦截方法
     *
     * @author JiaChaoYang
     * @date 2024/3/19 19:18
     */
    @Deprecated
    default List<AggregateBasicDBObject> executeAggregate(List<AggregateBasicDBObject> aggregateConditionList) {
        return aggregateConditionList;
    }

    /**
     * 管道拦截方法
     *
     * @author JiaChaoYang
     * @date 2024/3/19 19:18
     */
    default List<Bson> executeAggregates(List<Bson> aggregateConditionList) {
        return aggregateConditionList;
    }

    /**
     * 统计拦截方法
     *
     * @author JiaChaoYang
     * @date 2024/3/19 19:18
     */
    default MutablePair<BasicDBObject, CountOptions> executeCount(BasicDBObject queryBasic, CountOptions countOptions) {
        return new MutablePair<>(queryBasic, countOptions);
    }

    /**
     * 批量操作拦截方法
     *
     * @author JiaChaoYang
     * @date 2024/3/19 19:19
     */
    default List<WriteModel<Document>> executeBulkWrite(List<WriteModel<Document>> writeModelList) {
        return writeModelList;
    }

    /**
     * 添加拦截方法
     *
     * @param documentList 经过添加方法的值
     * @return java.util.List<org.bson.Document>
     * @author JiaChaoYang
     * @date 2024/3/17 0:37
     */
    default List<Document> executeSave(List<Document> documentList, MongoCollection<Document> collection) {
        return documentList;
    }

    /**
     * 删除拦截方法
     *
     * @author JiaChaoYang
     * @date 2024/3/19 19:18
     */
    default Bson executeRemove(Bson filter, MongoCollection<Document> collection) {
        return filter;
    }

    /**
     * 修改拦截方法
     *
     * @author JiaChaoYang
     * @date 2024/3/19 19:18
     */
    default MutablePair<Bson, Bson> executeUpdate(Bson queryBasic, Bson updateBasic, MongoCollection<Document> collection) {
        return new MutablePair<>(queryBasic, updateBasic);
    }

    /**
     * 查询拦截方法
     *
     * @author JiaChaoYang
     * @date 2024/3/19 19:18
     */
    default QueryParam executeQuery(Bson queryBasic, BasicDBObject projectionList, BasicDBObject sortCond, MongoCollection<Document> collection) {
        return new QueryParam(queryBasic, projectionList, sortCond);
    }

    /**
     * 管道拦截方法
     *
     * @author JiaChaoYang
     * @date 2024/3/19 19:18
     */
    default List<AggregateBasicDBObject> executeAggregate(List<AggregateBasicDBObject> aggregateConditionList, MongoCollection<Document> collection) {
        return aggregateConditionList;
    }

    /**
     * 管道拦截方法
     *
     * @author JiaChaoYang
     * @date 2024/3/19 19:18
     */
    default List<Bson> executeAggregates(List<Bson> aggregateConditionList, MongoCollection<Document> collection) {
        return aggregateConditionList;
    }

    /**
     * 统计拦截方法
     *
     * @author JiaChaoYang
     * @date 2024/3/19 19:18
     */
    default MutablePair<BasicDBObject, CountOptions> executeCount(BasicDBObject queryBasic, CountOptions countOptions, MongoCollection<Document> collection) {
        return new MutablePair<>(queryBasic, countOptions);
    }

    /**
     * 批量操作拦截方法
     *
     * @author JiaChaoYang
     * @date 2024/3/19 19:19
     */
    default List<WriteModel<Document>> executeBulkWrite(List<WriteModel<Document>> writeModelList, MongoCollection<Document> collection) {
        return writeModelList;
    }

}
