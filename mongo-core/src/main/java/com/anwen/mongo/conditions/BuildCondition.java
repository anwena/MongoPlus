package com.anwen.mongo.conditions;

import cn.hutool.core.collection.CollUtil;
import com.anwen.mongo.conditions.accumulator.Accumulator;
import com.anwen.mongo.conditions.interfaces.aggregate.project.Projection;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.constant.IndexConstant;
import com.anwen.mongo.enums.CompareEnum;
import com.anwen.mongo.enums.LogicTypeEnum;
import com.anwen.mongo.enums.QueryOperatorEnum;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.BasicDBObject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * 条件构建
 *
 * @author JiaChaoYang
 **/
public class BuildCondition<T> {

    /**
     * 构建projection条件
     * @author JiaChaoYang
     * @date 2023/8/19 0:11
     */
    public static BasicDBObject buildProjection(List<Projection> projectionList){
        return new BasicDBObject(){{
            projectionList.forEach(projection -> {
                put(projection.getColumn(),projection.getValue());
            });
        }};
    }

    /**
     * 构建查询条件
     *
     * @author JiaChaoYang
     * @date 2023/6/25/025 1:48
     */
    public static BasicDBObject buildQueryCondition(List<CompareCondition> compareConditionList) {
        return new BasicDBObject() {{
            compareConditionList.stream().filter(compareCondition -> compareCondition.getType() == CompareEnum.QUERY.getKey()).collect(Collectors.toList()).forEach(compare -> {
                if (Objects.equals(compare.getCondition(), QueryOperatorEnum.LIKE.getValue()) && StringUtils.isNotBlank(String.valueOf(compare.getValue()))) {
                    put(compare.getColumn(), new BasicDBObject(SpecialConditionEnum.REGEX.getCondition(), compare.getValue()));
                } else if (Objects.equals(compare.getLogicType(), LogicTypeEnum.OR.getKey())) {
                    if (CollUtil.isEmpty(compare.getChildCondition())) {
                        compare.setChildCondition(Collections.singletonList(compare));
                    }
                    put(SpecialConditionEnum.OR.getCondition(), buildOrQueryCondition(compare.getChildCondition()));
                } else if (Objects.equals(compare.getLogicType(), LogicTypeEnum.NOR.getKey())) {
                    put(SpecialConditionEnum.NOR.getCondition(), buildQueryCondition(compare.getChildCondition()));
                } else if (Objects.equals(compare.getLogicType(), LogicTypeEnum.ELEMMATCH.getKey())) {
                    put(SpecialConditionEnum.ELEM_MATCH.getCondition(), buildOrQueryCondition(compare.getChildCondition()));
                } else if (Objects.equals(compare.getCondition(), QueryOperatorEnum.TEXT.getValue())) {
                    put(SpecialConditionEnum.TEXT.getCondition(), new BasicDBObject(SpecialConditionEnum.SEARCH.getCondition(), compare.getValue()));
                    IndexConstant.createIndex = compare.getColumn();
                } else {
                    put(compare.getColumn(), new BasicDBObject("$" + compare.getCondition(), compare.getValue()));
                }
            });
        }};
    }

    /**
     * 构建子条件
     *
     * @author JiaChaoYang
     * @date 2023/7/16 19:59
     */
    public static List<BasicDBObject> buildOrQueryCondition(List<CompareCondition> compareConditionList) {
        List<BasicDBObject> basicDBObjectList = new ArrayList<>();
        compareConditionList.forEach(compare -> {
            BasicDBObject basicDBObject = new BasicDBObject();
            if (Objects.equals(compare.getCondition(), QueryOperatorEnum.LIKE.getValue()) && StringUtils.isNotBlank(String.valueOf(compare.getValue()))) {
                basicDBObject.put(compare.getColumn(), new BasicDBObject(SpecialConditionEnum.REGEX.getCondition(), compare.getValue()));
            } else if (Objects.equals(compare.getCondition(), QueryOperatorEnum.AND.getValue())) {
                basicDBObjectList.add(buildQueryCondition(compare.getChildCondition()));
            } else if (Objects.equals(compare.getCondition(), QueryOperatorEnum.TEXT.getValue())) {
                basicDBObject.put(SpecialConditionEnum.TEXT.getCondition(), new BasicDBObject(SpecialConditionEnum.SEARCH.getCondition(), compare.getValue()));
                IndexConstant.createIndex = compare.getColumn();
            }else {
                basicDBObject.put(compare.getColumn(), new BasicDBObject("$" + compare.getCondition(), compare.getValue()));
            }
            basicDBObjectList.add(basicDBObject);
        });
        return basicDBObjectList;
    }

    /**
     * 构建group条件
     * @author JiaChaoYang
     * @date 2023/8/19 0:11
     */
    public static BasicDBObject buildGroup(List<Accumulator> accumulatorList){
        BasicDBObject basicDBObject = new BasicDBObject();
        accumulatorList.forEach(accumulator -> {
            basicDBObject.put(accumulator.getResultMappingField(),new BasicDBObject(){{
                put("$"+accumulator.getCondition(),"$"+accumulator.getField());
            }});
        });
        return basicDBObject;
    }

}
