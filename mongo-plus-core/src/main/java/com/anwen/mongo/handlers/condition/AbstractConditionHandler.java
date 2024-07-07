package com.anwen.mongo.handlers.condition;

import com.anwen.mongo.bson.MongoPlusBasicDBObject;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.toolkit.CollUtil;
import com.mongodb.BasicDBObject;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static com.anwen.mongo.enums.QueryOperatorEnum.isQueryOperator;

/**
 * 抽象的条件处理器
 *
 * @author anwen
 * @date 2024/6/30 下午3:58
 */
public abstract class AbstractConditionHandler {

    public BasicDBObject queryCondition(List<CompareCondition> compareConditionList) {
        MongoPlusBasicDBObject mongoPlusBasicDBObject = new MongoPlusBasicDBObject();
        if (CollUtil.isNotEmpty(compareConditionList)) {
            compareConditionList.stream().filter(compareCondition -> isQueryOperator(compareCondition.getCondition())).forEach(compareCondition -> queryCondition(compareCondition,mongoPlusBasicDBObject));
        }
        return mongoPlusBasicDBObject;
    }

    public BasicDBObject projectionCondition(List<Projection> projectionList){
        return new BasicDBObject(){{
            if (CollUtil.isNotEmpty(projectionList)) {
                projectionList.forEach(projection -> put(projection.getColumn(), projection.getValue()));
            }
        }};
    }

    /**
     * 构建projection条件
     * @author JiaChaoYang
     * @date 2023/8/19 0:11
     */
    public BasicDBObject buildProjection(List<Projection> projectionList){
        return new BasicDBObject(){{
            if (CollUtil.isNotEmpty(projectionList)) {
                projectionList.forEach(projection -> put(projection.getColumn(), projection.getValue()));
            }
        }};
    }

    /**
     * 构建更新值
     *
     * @author JiaChaoYang
     * @date 2023/7/9 22:16
     */
    public static BasicDBObject buildUpdateValue(List<CompareCondition> compareConditionList) {
        return new BasicDBObject() {{
            compareConditionList.stream().filter(compareCondition -> !isQueryOperator(compareCondition.getCondition())).collect(Collectors.toList()).forEach(compare -> put(compare.getColumn(), compare.getValue()));
        }};
    }

    public static BasicDBObject buildPushUpdateValue(List<CompareCondition> compareConditionList) {
        List<CompareCondition> conditionList = compareConditionList.stream().filter(compareCondition -> !isQueryOperator(compareCondition.getCondition())).collect(Collectors.toList());
        List<String> columnList = conditionList.stream().map(CompareCondition::getColumn).distinct().collect(Collectors.toList());
        //必须得这样做，为了兼容追加参数只追加一个、而且不是数组的情况
        return new BasicDBObject(){{
            columnList.forEach(column -> {
                List<Object> valueList = conditionList.stream().filter(condition -> Objects.equals(condition.getColumn(), column)).map(CompareCondition::getValue).collect(Collectors.toList());
                put(column,new BasicDBObject(SpecialConditionEnum.EACH.getCondition(),valueList));
            });
        }};
    }

    public abstract BasicDBObject queryCondition(CompareCondition compareCondition,MongoPlusBasicDBObject mongoPlusBasicDBObject);

}
