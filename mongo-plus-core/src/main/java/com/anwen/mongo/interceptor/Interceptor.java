package com.anwen.mongo.interceptor;

import com.anwen.mongo.model.AggregateBasicDBObject;
import com.anwen.mongo.model.MutablePair;
import com.anwen.mongo.model.QueryParam;
import com.mongodb.BasicDBObject;
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
     * 添加拦截方法
     * @param documentList 经过添加方法的值
     * @return java.util.List<org.bson.Document>
     * @author JiaChaoYang
     * @date 2024/3/17 0:37
    */
    default List<Document> executeSave(List<Document> documentList){
        return documentList;
    };

    default Bson executeRemove(Bson filter){
        return filter;
    }

    default MutablePair<Bson,Bson> executeUpdate(Bson queryBasic, Bson updateBasic){
        return new MutablePair<>(queryBasic, updateBasic);
    }

    default QueryParam executeQuery(Bson queryBasic, BasicDBObject projectionList, BasicDBObject sortCond){
        return new QueryParam(queryBasic, projectionList, sortCond);
    }

    default List<AggregateBasicDBObject> executeAggregate(List<AggregateBasicDBObject> aggregateConditionList){
        return aggregateConditionList;
    }

    default MutablePair<BasicDBObject,CountOptions> executeCount(BasicDBObject queryBasic, CountOptions countOptions){
        return new MutablePair<>(queryBasic, countOptions);
    }

    default List<WriteModel<Document>> executeBulkWrite(List<WriteModel<Document>> writeModelList){
        return writeModelList;
    }

}
