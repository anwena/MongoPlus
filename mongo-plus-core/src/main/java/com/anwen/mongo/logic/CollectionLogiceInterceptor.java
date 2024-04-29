package com.anwen.mongo.logic;

import com.anwen.mongo.cache.global.ClassLogicDeleteCache;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.model.MutablePair;
import com.anwen.mongo.model.QueryParam;
import com.mongodb.BasicDBObject;
import com.mongodb.client.model.CountOptions;
import com.mongodb.client.model.UpdateManyModel;
import com.mongodb.client.model.WriteModel;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * 逻辑删除拦截器
 *
 * @author loser
 * @date 2024/4/30
 */
public class CollectionLogiceInterceptor implements Interceptor {

    @Override
    public Bson executeRemove(Bson filter) {

        try {
            Class<?> clazz = ClassLogicDeleteCache.getLogicCollection();
            if (LogicDeleteHandler.close() || Objects.isNull(clazz)) {
                return Interceptor.super.executeRemove(filter);
            }
            return LogicDeleteHandler.doBsonLogicDel(filter, clazz);
        } finally {
            ClassLogicDeleteCache.clear();
        }

    }

    @Override
    public MutablePair<Bson, Bson> executeUpdate(Bson queryBasic, Bson updateBasic) {

        try {
            Class<?> clazz = ClassLogicDeleteCache.getLogicCollection();
            if (LogicDeleteHandler.close() || Objects.isNull(clazz)) {
                return Interceptor.super.executeUpdate(queryBasic, updateBasic);
            }
            Bson query = LogicDeleteHandler.doBsonLogicDel(queryBasic, clazz);
            return new MutablePair<>(query, updateBasic);
        } finally {
            ClassLogicDeleteCache.clear();
        }

    }

    @Override
    public QueryParam executeQuery(Bson queryBasic, BasicDBObject projectionList, BasicDBObject sortCond) {

        try {
            Class<?> clazz = ClassLogicDeleteCache.getLogicCollection();
            if (LogicDeleteHandler.close() || Objects.isNull(clazz)) {
                return Interceptor.super.executeQuery(queryBasic, projectionList, sortCond);
            }
            BasicDBObject query = (BasicDBObject) LogicDeleteHandler.doBsonLogicDel(queryBasic, clazz);
            return new QueryParam(query, projectionList, sortCond);
        } finally {
            ClassLogicDeleteCache.clear();
        }

    }

    @Override
    public MutablePair<BasicDBObject, CountOptions> executeCount(BasicDBObject queryBasic, CountOptions countOptions) {

        try {
            Class<?> clazz = ClassLogicDeleteCache.getLogicCollection();
            if (LogicDeleteHandler.close() || Objects.isNull(clazz)) {
                return Interceptor.super.executeCount(queryBasic, countOptions);
            }
            BasicDBObject query = (BasicDBObject) LogicDeleteHandler.doBsonLogicDel(queryBasic, clazz);
            return new MutablePair<>(query, countOptions);
        } finally {
            ClassLogicDeleteCache.clear();
        }

    }

    @Override
    @SuppressWarnings("all")
    public List<WriteModel<Document>> executeBulkWrite(List<WriteModel<Document>> writeModelList) {

        try {
            Class<?> clazz = ClassLogicDeleteCache.getLogicCollection();
            if (LogicDeleteHandler.close() || Objects.isNull(clazz)) {
                return Interceptor.super.executeBulkWrite(writeModelList);
            }
            return writeModelList.stream().map(item -> {
                if (item instanceof UpdateManyModel) {
                    UpdateManyModel umm = (UpdateManyModel) item;
                    Bson filter = LogicDeleteHandler.doBsonLogicDel(umm.getFilter(), clazz);
                    return new UpdateManyModel<Document>(filter, umm.getUpdate());
                }
                return item;
            }).collect(Collectors.toList());
        } finally {
            ClassLogicDeleteCache.clear();
        }

    }

}
