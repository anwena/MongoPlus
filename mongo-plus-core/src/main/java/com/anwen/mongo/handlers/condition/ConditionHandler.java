package com.anwen.mongo.handlers.condition;

import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.mongodb.BasicDBObject;

import java.util.List;

/**
 * 条件处理器
 *
 * @author anwen
 * @date 2024/6/30 下午3:57
 */
interface ConditionHandler {

    /**
     * 查询条件
     * @param compareConditionList 条件集合
     * @return {@link BasicDBObject}
     * @author anwen
     * @date 2024/6/30 下午4:01
     */
    BasicDBObject queryCondition(List<CompareCondition> compareConditionList);

}
