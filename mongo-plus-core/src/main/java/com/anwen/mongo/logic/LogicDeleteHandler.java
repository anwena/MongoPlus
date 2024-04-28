package com.anwen.mongo.logic;

import com.anwen.mongo.cache.global.ClassLogicDeleteCache;
import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.query.LambdaQueryChainWrapper;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.model.LogicDeleteResult;
import com.anwen.mongo.toolkit.ChainWrappers;
import com.mongodb.BasicDBObject;
import org.bson.BsonDocument;
import org.bson.BsonString;
import org.bson.conversions.Bson;

import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * 逻辑删除处理类
 *
 * @author loser
 * @date 2024/4/29
 */
public interface LogicDeleteHandler {

    /**
     * 是否关闭逻辑删除功能
     */
    static boolean close() {
        return !ClassLogicDeleteCache.open;
    }

    static Map<Class<?>, LogicDeleteResult> mapper() {
        return ClassLogicDeleteCache.logicDeleteResultHashMap;
    }

    /**
     * bson 对象添加逻辑未删除条件
     *
     * @param query 查询条件
     * @param clazz 目标文档
     * @param <T>   文档类型
     * @return 添加逻辑未删除的条件对象
     */
    static <T> Bson doBsonLogicDel(Bson query, Class<T> clazz) {

        if (close()) {
            return query;
        }
        LogicDeleteResult result = mapper().get(clazz);
        if (Objects.isNull(result)) {
            return query;
        }
        if (Objects.isNull(query)) {
            LambdaQueryChainWrapper<T> wrapper = ChainWrappers.lambdaQueryChain(null, clazz);
            wrapper.eq(result.getColumn(), result.getLogicNotDeleteValue());
            return BuildCondition.buildQueryCondition(wrapper.getCompareList());
        }
        if (query instanceof BasicDBObject) {
            BasicDBObject bdb = (BasicDBObject) query;
            bdb.put(result.getColumn(), new BsonString(result.getLogicNotDeleteValue()));
            return bdb;
        }
        BsonDocument bsonDocument = query.toBsonDocument();
        bsonDocument.append(result.getColumn(), new BsonString(result.getLogicNotDeleteValue()));
        return bsonDocument;

    }

    /**
     * 给 wrapper 对象添加逻辑未删除对象
     *
     * @param queryChainWrapper wrapper 条件包裹对象
     * @param clazz             目标文档
     * @param <T>               文档类型
     * @return 添加逻辑未删除的条件集合
     */
    @SuppressWarnings("unchecked")
    static <T> List<CompareCondition> doWrapperLogicDel(QueryChainWrapper<T, ?> queryChainWrapper, Class clazz) {

        if (close()) {
            if (Objects.isNull(queryChainWrapper)) {
                return null;
            }
            return queryChainWrapper.getCompareList();
        }
        LogicDeleteResult result = ClassLogicDeleteCache.logicDeleteResultHashMap.get(clazz);
        if (Objects.isNull(result)) {
            if (Objects.isNull(queryChainWrapper)) {
                return null;
            }
            return queryChainWrapper.getCompareList();
        }
        if (Objects.isNull(queryChainWrapper)) {
            queryChainWrapper = ChainWrappers.lambdaQueryChain(null, clazz);
        }
        queryChainWrapper.eq(result.getColumn(), result.getLogicNotDeleteValue());
        return queryChainWrapper.getCompareList();

    }

}
