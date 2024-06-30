package com.anwen.mongo.handlers.condition;

import com.anwen.mongo.bson.MongoPlusBasicDBObject;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.toolkit.CollUtil;
import com.mongodb.BasicDBObject;

import java.util.List;

import static com.anwen.mongo.enums.QueryOperatorEnum.isQueryOperator;

/**
 * 抽象的条件处理器
 *
 * @author anwen
 * @date 2024/6/30 下午3:58
 */
public abstract class AbstractConditionHandler implements ConditionHandler {

    @Override
    public BasicDBObject queryCondition(List<CompareCondition> compareConditionList) {
        MongoPlusBasicDBObject mongoPlusBasicDBObject = new MongoPlusBasicDBObject();
        if (CollUtil.isNotEmpty(compareConditionList)) {
            compareConditionList.stream().filter(compareCondition -> isQueryOperator(compareCondition.getCondition())).forEach(compareCondition -> queryCondition(compareCondition,mongoPlusBasicDBObject));
        }
        return mongoPlusBasicDBObject;
    }

    public abstract BasicDBObject queryCondition(CompareCondition compareCondition,MongoPlusBasicDBObject mongoPlusBasicDBObject);

}
