package com.anwen.mongo.handlers.condition;

import com.anwen.mongo.annotation.comm.FieldEncrypt;
import com.anwen.mongo.bson.MongoPlusBasicDBObject;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.domain.MongoPlusException;
import com.anwen.mongo.enums.QueryOperatorEnum;
import com.anwen.mongo.enums.TypeEnum;
import com.anwen.mongo.toolkit.EncryptorUtil;
import com.anwen.mongo.toolkit.Filters;
import com.mongodb.BasicDBObject;
import org.bson.BsonType;
import org.bson.conversions.Bson;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * 构建条件
 *
 * @author anwen
 * @date 2024/6/30 下午4:07
 */
public class BuildCondition extends AbstractConditionHandler {

    @Override
    @SuppressWarnings("unchecked")
    public BasicDBObject queryCondition(CompareCondition compareCondition, MongoPlusBasicDBObject mongoPlusBasicDBObject) {
        QueryOperatorEnum query = QueryOperatorEnum.getQueryOperator(compareCondition.getCondition());
        Field originalField = compareCondition.getOriginalField();
        if (originalField != null && originalField.isAnnotationPresent(FieldEncrypt.class)){
            compareCondition.setValue(EncryptorUtil.encrypt(originalField.getAnnotation(FieldEncrypt.class),compareCondition.getValue()));
        }
        switch (Objects.requireNonNull(query)){
            case EQ:
                mongoPlusBasicDBObject.put(Filters.eq(compareCondition.getColumn(), compareCondition.getValue()));
                break;
            case NE:
                mongoPlusBasicDBObject.put(Filters.ne(compareCondition.getColumn(), compareCondition.getValue()));
                break;
            case GT:
                mongoPlusBasicDBObject.put(Filters.gt(compareCondition.getColumn(), compareCondition.getValue()));
                break;
            case LT:
                mongoPlusBasicDBObject.put(Filters.lt(compareCondition.getColumn(), compareCondition.getValue()));
                break;
            case GTE:
                mongoPlusBasicDBObject.put(Filters.gte(compareCondition.getColumn(), compareCondition.getValue()));
                break;
            case LTE:
                mongoPlusBasicDBObject.put(Filters.lte(compareCondition.getColumn(), compareCondition.getValue()));
                break;
            case REGEX:
            case LIKE:
                mongoPlusBasicDBObject.put(Filters.regex(compareCondition.getColumn(), (String) compareCondition.getValue()));
                break;
            case IN:
                mongoPlusBasicDBObject.put(Filters.in(compareCondition.getColumn(), compareCondition.getValue()));
                break;
            case NIN:
                mongoPlusBasicDBObject.put(Filters.nin(compareCondition.getColumn(), compareCondition.getValue()));
                break;
            case AND:
                List<Bson> andBsonList = new ArrayList<>();
                ((List<CompareCondition>) compareCondition.getValue()).forEach(andCompareCondition -> andBsonList.add(buildQueryCondition(andCompareCondition)));
                mongoPlusBasicDBObject.put(Filters.and(andBsonList));
                break;
            case OR:
                List<Bson> orBsonList = new ArrayList<>();
                ((List<CompareCondition>) compareCondition.getValue()).forEach(andCompareCondition -> orBsonList.add(buildQueryCondition(andCompareCondition)));
                mongoPlusBasicDBObject.put(Filters.or(orBsonList));
                break;
            case NOR:
                List<Bson> norBsonList = new ArrayList<>();
                ((List<CompareCondition>) compareCondition.getValue()).forEach(andCompareCondition -> norBsonList.add(buildQueryCondition(andCompareCondition)));
                mongoPlusBasicDBObject.put(Filters.nor(norBsonList));
                break;
            case TYPE:
                Object typeValue = compareCondition.getValue();
                if (typeValue instanceof String){
                    mongoPlusBasicDBObject.put(Filters.type(compareCondition.getColumn(), (String) typeValue));
                    break;
                }
                if (typeValue instanceof TypeEnum){
                    typeValue = ((TypeEnum) typeValue).getTypeCode();
                }
                mongoPlusBasicDBObject.put(Filters.type(compareCondition.getColumn(), BsonType.findByValue((Integer) typeValue)));
                break;
            case EXISTS:
                mongoPlusBasicDBObject.put(Filters.exists(compareCondition.getColumn(), (Boolean) compareCondition.getValue()));
                break;
            case NOT:
                mongoPlusBasicDBObject.put(Filters.not(buildQueryCondition((CompareCondition) compareCondition.getValue())));
                break;
            case EXPR:
                mongoPlusBasicDBObject.put(Filters.expr(buildQueryCondition((CompareCondition) compareCondition.getValue())));
                break;
            case MOD:
                List<Long> modList = (List<Long>) compareCondition.getValue();
                if (modList.size() < 2){
                    throw new MongoPlusException("Mod requires modulus and remainder");
                }
                mongoPlusBasicDBObject.put(Filters.mod(compareCondition.getColumn(), modList.get(0),modList.get(1)));
                break;
            case ELEM_MATCH:
                mongoPlusBasicDBObject.put(Filters.elemMatch(compareCondition.getColumn(),queryCondition((List<CompareCondition>) compareCondition.getValue())));
                break;
            case ALL:
                mongoPlusBasicDBObject.put(Filters.all(compareCondition.getColumn(), compareCondition.getValue()));
                break;
            case TEXT:
                mongoPlusBasicDBObject.put(Filters.text(compareCondition.getValue().toString()));
                break;
            case WHERE:
                mongoPlusBasicDBObject.put(Filters.where((String) compareCondition.getValue()));
                break;
            case SIZE:
                mongoPlusBasicDBObject.put(Filters.size(compareCondition.getColumn(), (Integer) compareCondition.getValue()));
                break;
            case BITS_ALL_CLEAR:
                mongoPlusBasicDBObject.put(Filters.bitsAllClear(compareCondition.getColumn(), (Integer) compareCondition.getValue()));
                break;
            case BITS_ALL_SET:
                mongoPlusBasicDBObject.put(Filters.bitsAllSet(compareCondition.getColumn(), (Integer) compareCondition.getValue()));
                break;
            case BITS_ANY_CLEAR:
                mongoPlusBasicDBObject.put(Filters.bitsAnyClear(compareCondition.getColumn(), (Integer) compareCondition.getValue()));
                break;
            case BITS_ANY_SET:
                mongoPlusBasicDBObject.put(Filters.bitsAnySet(compareCondition.getColumn(), (Integer) compareCondition.getValue()));
                break;
        }
        return mongoPlusBasicDBObject;
    }

    public BasicDBObject buildQueryCondition(CompareCondition compareCondition){
        return queryCondition(compareCondition,new MongoPlusBasicDBObject());
    }

}
