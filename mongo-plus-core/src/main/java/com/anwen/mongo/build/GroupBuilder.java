package com.anwen.mongo.build;

import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.accumulator.Accumulator;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.model.GroupField;
import com.anwen.mongo.toolkit.CollUtil;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.BasicDBObject;

import java.util.List;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description 使用建造者构建group的条件
 * @date 2023-11-13 17:41
 **/
public class GroupBuilder {

    private BasicDBObject basicDBObject;

    public GroupBuilder() {
        this.basicDBObject = new BasicDBObject();
    }

    public GroupBuilder withAccumulatorList(List<Accumulator> accumulatorList) {
        if (CollUtil.isNotEmpty(accumulatorList)) {
            basicDBObject = BuildCondition.buildGroup(accumulatorList);
        }
        return this;
    }

    public GroupBuilder withId(String id) {
        if (StringUtils.isNotBlank(id)) {
            basicDBObject.put(SqlOperationConstant._ID, "$" + id);
        }
        return this;
    }

    public GroupBuilder withIdList(List<GroupField> idList) {
        if (CollUtil.isNotEmpty(idList)) {
            for (GroupField groupField : idList) {
                basicDBObject.put(SqlOperationConstant._ID,new BasicDBObject(){{
                    put(groupField.getGroupField(),"$" + groupField.getField());
                }});
            }
        }
        return this;
    }

    public GroupBuilder withIdAccumulator(List<Accumulator> idAccumulator) {
        if (CollUtil.isNotEmpty(idAccumulator)) {
            basicDBObject.put(SqlOperationConstant._ID, BuildCondition.buildGroup(idAccumulator));
        }
        return this;
    }

    public BasicDBObject build() {
        return basicDBObject;
    }

}
