package com.anwen.mongo.conditions;

import com.anwen.mongo.bson.MongoPlusBasicDBObject;
import com.anwen.mongo.conditions.accumulator.Accumulator;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.AddFields;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.ReplaceRoot;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.domain.MongoPlusException;
import com.anwen.mongo.enums.QueryOperatorEnum;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.enums.TypeEnum;
import com.anwen.mongo.toolkit.CollUtil;
import com.anwen.mongo.toolkit.Filters;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.BasicDBObject;
import org.bson.BsonType;
import org.bson.conversions.Bson;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static com.anwen.mongo.enums.QueryOperatorEnum.isQueryOperator;

/**
 * 条件构建
 *
 * @author JiaChaoYang
 **/
public class BuildCondition {

    /**
     * 构建projection条件
     * @author JiaChaoYang
     * @date 2023/8/19 0:11
     */
    public static BasicDBObject buildProjection(List<Projection> projectionList){
        return new BasicDBObject(){{
            if (CollUtil.isNotEmpty(projectionList)) {
                projectionList.forEach(projection -> put(projection.getColumn(), projection.getValue()));
            }
        }};
    }

    /**
     * 构建查询条件
     *
     * @author JiaChaoYang
     * @date 2023/6/25/025 1:48
     */
    public static BasicDBObject buildQueryCondition(List<CompareCondition> compareConditionList){
        MongoPlusBasicDBObject mongoPlusBasicDBObject = new MongoPlusBasicDBObject();
        if (CollUtil.isNotEmpty(compareConditionList)) {
            compareConditionList.stream().filter(compareCondition -> isQueryOperator(compareCondition.getCondition())).forEach(compareCondition -> buildQueryCondition(compareCondition,mongoPlusBasicDBObject));
        }
        return mongoPlusBasicDBObject;
    }

    public static BasicDBObject buildQueryCondition(CompareCondition compareCondition){
        return buildQueryCondition(compareCondition,new MongoPlusBasicDBObject());
    }

    @SuppressWarnings("unchecked")
    public static BasicDBObject buildQueryCondition(CompareCondition compareCondition,MongoPlusBasicDBObject mongoPlusBasicDBObject){
        QueryOperatorEnum query = QueryOperatorEnum.getQueryOperator(compareCondition.getCondition());
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
                mongoPlusBasicDBObject.put(Filters.elemMatch(compareCondition.getColumn(),buildQueryCondition((List<CompareCondition>) compareCondition.getValue())));
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

    public static BasicDBObject buildReplaceRoot(Boolean reserveOriginalDocument,List<ReplaceRoot> replaceRootList){
        return new BasicDBObject(){{
            if (replaceRootList.size() == 1 && !reserveOriginalDocument){
                put("newRoot","$"+replaceRootList.get(0).getField());
            }else {
                put("newRoot",new BasicDBObject(){{
                    put("$mergeObjects",new ArrayList<Object>(){{
                        if (reserveOriginalDocument){
                            add("$$ROOT");
                        }
                        for (ReplaceRoot replaceRoot : replaceRootList) {
                            add(new BasicDBObject(){{
                                put(replaceRoot.getResultMappingField(),"$"+replaceRoot.getField());
                            }});
                        }
                    }});
                }});
            }
        }};
    }

    /**
     * 构建group条件
     * @author JiaChaoYang
     * @date 2023/8/19 0:11
     */
    public static MongoPlusBasicDBObject buildGroup(List<Accumulator> accumulatorList){
        return new MongoPlusBasicDBObject(){{
            accumulatorList.forEach(accumulator -> put(accumulator.getResultMappingField(),new BasicDBObject(){{
                put("$"+accumulator.getCondition(),accumulator.getField());
            }}));
        }};
    }

    /**
     * 构建addFields
     * @author JiaChaoYang
     * @date 2023/8/20 1:08
    */
    public static BasicDBObject buildAddFields(List<AddFields> addFieldsList){
        return new BasicDBObject(){{
           addFieldsList.forEach(addFields -> {
               put(addFields.getResultMappingField(),addFields.getField());
           });
        }};
    }

    /**
     * 构建unwind
     * @author JiaChaoYang
     * @date 2023/8/20 1:11
    */
    public static BasicDBObject buildUnwind(Boolean preserveNullAndEmptyArrays,String field){
        return new BasicDBObject(){{
           put("path","$"+field);
           put("preserveNullAndEmptyArrays",preserveNullAndEmptyArrays);
        }};
    }

    public static BasicDBObject buildSample(long size){
        return new BasicDBObject(){{
            put("size",size);
        }};
    }

    /**
     * 构建out
     * @author JiaChaoYang
     * @date 2023/8/20 1:38
    */
    public static BasicDBObject buildOut(String db,String coll){
        return new BasicDBObject(){{
           if (StringUtils.isNotBlank(db)){
               put("db",db);
               put("coll",coll);
           }
        }};
    }

}
