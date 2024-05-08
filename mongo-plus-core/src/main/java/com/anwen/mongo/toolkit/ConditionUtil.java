package com.anwen.mongo.toolkit;

import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.model.MutablePair;
import com.mongodb.BasicDBObject;
import org.bson.Document;

import java.util.List;

public class ConditionUtil {

    /**
     * 将条件转为MongoDB可用条件
     * @author anwen
     * @date 2024/5/4 下午1:16
     */
    public static MutablePair<BasicDBObject,BasicDBObject> getUpdateCondition(List<CompareCondition> compareConditionList, Object sourceObj, MongoConverter mongoConverter){
        BasicDBObject queryBasic = BuildCondition.buildQueryCondition(compareConditionList);
        Document document = mongoConverter.writeByUpdate(sourceObj);
        document.remove(SqlOperationConstant._ID);
        BasicDBObject updateField = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), document);
        return new MutablePair<>(queryBasic,updateField);
    }

    /**
     * 将实体构建为Id条件
     * @param sourceObj 实体
     * @param mongoConverter 转换器
     * @return {@link MutablePair< BasicDBObject, BasicDBObject>}
     * @author anwen
     * @date 2024/5/4 下午1:15
     */
    public static MutablePair<BasicDBObject,BasicDBObject> getUpdate(Object sourceObj,MongoConverter mongoConverter) {
        Document document = mongoConverter.writeByUpdate(sourceObj);
        BasicDBObject filter = ExecuteUtil.getFilter(document);
        BasicDBObject update = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), document);
        return new MutablePair<>(filter,update);
    }

}
