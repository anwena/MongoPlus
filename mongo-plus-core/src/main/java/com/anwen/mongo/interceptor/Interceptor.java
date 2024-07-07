package com.anwen.mongo.interceptor;

import com.anwen.mongo.enums.ExecuteMethodEnum;
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
     * 前置处理
     * @param executeMethodEnum 执行类型
     * @param source 值
     * @param collection 集合对象
     * @return {@link java.lang.Object[]}
     * @author anwen
     * @date 2024/6/27 下午4:38
     */
    default Object[] beforeExecute(ExecuteMethodEnum executeMethodEnum, Object[] source, MongoCollection<Document> collection){
        return source;
    }

    /**
     * 后置处理
     * @param executeMethodEnum 执行类型
     * @param source 参数值
     * @param result 返回值
     * @param collection 集合对象
     * @return {@link java.lang.Object}
     * @author anwen
     * @date 2024/6/27 下午5:20
     */
    default Object afterExecute(ExecuteMethodEnum executeMethodEnum, Object[] source,Object result, MongoCollection<Document> collection){
        return result;
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
     * @param filter 条件
     * @return {@link org.bson.conversions.Bson}
     * @author anwen
     * @date 2024/6/27 下午4:38
     */
    default Bson executeRemove(Bson filter) {
        return filter;
    }

    /**
     * 修改拦截方法
     * @param queryBasic 条件
     * @param updateBasic 更新
     * @return {@link com.anwen.mongo.model.MutablePair<org.bson.conversions.Bson,org.bson.conversions.Bson>}
     * @author anwen
     * @date 2024/6/27 下午4:38
     */
    @Deprecated
    default MutablePair<Bson, Bson> executeUpdate(Bson queryBasic, Bson updateBasic) {
        return new MutablePair<>(queryBasic, updateBasic);
    }

    /**
     * 修改拦截方法
     * @param updatePairList 值 left=查询条件 right=更新条件
     * @return {@link java.util.List<com.anwen.mongo.model.MutablePair<org.bson.conversions.Bson,org.bson.conversions.Bson>>}
     * @author anwen
     * @date 2024/6/27 下午4:37
     */
    default List<MutablePair<Bson,Bson>> executeUpdate(List<MutablePair<Bson,Bson>> updatePairList){
        return updatePairList;
    }

    /**
     * 查询拦截方法
     * @param queryBasic 条件
     * @param projectionList 显隐字段
     * @param sortCond 排序
     * @return {@link com.anwen.mongo.model.QueryParam}
     * @author anwen
     * @date 2024/6/27 下午4:39
     */
    default QueryParam executeQuery(Bson queryBasic, BasicDBObject projectionList, BasicDBObject sortCond) {
        return new QueryParam(queryBasic, projectionList, sortCond);
    }

    /**
     * 管道拦截方法
     * @param aggregateConditionList 管道对象
     * @return {@link java.util.List<com.anwen.mongo.model.AggregateBasicDBObject>}
     * @author anwen
     * @date 2024/6/27 下午4:39
     */
    @Deprecated
    default List<AggregateBasicDBObject> executeAggregate(List<AggregateBasicDBObject> aggregateConditionList) {
        return aggregateConditionList;
    }

    /**
     * 管道拦截方法
     * @param aggregateConditionList 管道对象
     * @return {@link java.util.List<org.bson.conversions.Bson>}
     * @author anwen
     * @date 2024/6/27 下午4:40
     */
    default List<Bson> executeAggregates(List<Bson> aggregateConditionList) {
        return aggregateConditionList;
    }

    /**
     * 统计拦截方法
     * @param queryBasic 条件
     * @param countOptions 选项
     * @return {@link com.anwen.mongo.model.MutablePair<com.mongodb.BasicDBObject,com.mongodb.client.model.CountOptions>} left = 条件 right = 选项
     * @author anwen
     * @date 2024/6/27 下午4:40
     */
    default MutablePair<BasicDBObject, CountOptions> executeCount(BasicDBObject queryBasic, CountOptions countOptions) {
        return new MutablePair<>(queryBasic, countOptions);
    }

    /**
     * 批量操作拦截方法
     * @param writeModelList 操作对象，{@link com.mongodb.client.model.InsertOneModel}或{@link com.mongodb.client.model.UpdateManyModel}
     * @return {@link java.util.List<com.mongodb.client.model.WriteModel<org.bson.Document>>}
     * @author anwen
     * @date 2024/6/27 下午4:41
     */
    default List<WriteModel<Document>> executeBulkWrite(List<WriteModel<Document>> writeModelList) {
        return writeModelList;
    }

    /**
     * 添加拦截方法
     * @param documentList 添加的值
     * @return {@link java.util.List<org.bson.Document>}
     * @author anwen
     * @date 2024/6/27 下午4:42
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
    @Deprecated
    default MutablePair<Bson, Bson> executeUpdate(Bson queryBasic, Bson updateBasic, MongoCollection<Document> collection) {
        return new MutablePair<>(queryBasic, updateBasic);
    }

    /**
     * 修改拦截方法
     * @param updatePairList 值 left=查询条件 right=更新条件
     * @return {@link java.util.List<com.anwen.mongo.model.MutablePair<org.bson.conversions.Bson,org.bson.conversions.Bson>>}
     * @author anwen
     * @date 2024/6/27 下午4:37
     */
    default List<MutablePair<Bson,Bson>> executeUpdate(List<MutablePair<Bson,Bson>> updatePairList, MongoCollection<Document> collection){
        return updatePairList;
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
