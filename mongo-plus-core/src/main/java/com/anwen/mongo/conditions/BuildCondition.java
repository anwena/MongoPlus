package com.anwen.mongo.conditions;

import com.anwen.mongo.conditions.accumulator.Accumulator;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.AddFields;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.ReplaceRoot;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.constant.IndexConstant;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.enums.CompareEnum;
import com.anwen.mongo.enums.LogicTypeEnum;
import com.anwen.mongo.enums.QueryOperatorEnum;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.toolkit.ObjectIdUtil;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.BasicDBObject;
import org.bson.types.ObjectId;

import java.util.*;
import java.util.stream.Collectors;

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
            if (projectionList != null && !projectionList.isEmpty()) {
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
    public static BasicDBObject buildQueryCondition(List<CompareCondition> compareConditionList) {
        return new BasicDBObject() {{
            if (compareConditionList != null && !compareConditionList.isEmpty()) {
                compareConditionList.stream().filter(compareCondition -> compareCondition.getType() == CompareEnum.QUERY.getKey()).collect(Collectors.toList()).forEach(compare -> {
                    if (Objects.equals(compare.getCondition(), QueryOperatorEnum.LIKE.getValue()) && StringUtils.isNotBlank(String.valueOf(compare.getValue()))) {
                        put(compare.getColumn(), new BasicDBObject(SpecialConditionEnum.REGEX.getCondition(), compare.getValue()));
                    } else if (Objects.equals(compare.getLogicType(), LogicTypeEnum.OR.getKey())) {
                        if (null == compare.getChildCondition() || compare.getChildCondition().isEmpty()) {
                            compare.setChildCondition(Collections.singletonList(compare));
                        }
                        put(SpecialConditionEnum.OR.getCondition(), buildOrQueryCondition(compare.getChildCondition()));
                    } else if (Objects.equals(compare.getLogicType(), LogicTypeEnum.NOR.getKey())) {
                        put(SpecialConditionEnum.NOR.getCondition(), buildQueryCondition(compare.getChildCondition()));
                    } else if (Objects.equals(compare.getLogicType(), LogicTypeEnum.ELEMMATCH.getKey())) {
                        put(compare.getColumn(),new BasicDBObject(SpecialConditionEnum.ELEM_MATCH.getCondition(),buildQueryCondition(compare.getChildCondition())));
                    } else if (Objects.equals(compare.getCondition(), QueryOperatorEnum.TEXT.getValue())) {
                        put(SpecialConditionEnum.TEXT.getCondition(), new BasicDBObject(SpecialConditionEnum.SEARCH.getCondition(), compare.getValue()));
                        IndexConstant.createIndex = compare.getColumn();
                    } else if (Objects.equals(compare.getColumn(), SqlOperationConstant._ID)) {
                        if (SpecialConditionEnum.IN.getCondition().equals("$" + compare.getCondition())) {
                            put(compare.getColumn(), new BasicDBObject("$" + compare.getCondition(), ObjectIdUtil.convertObjectId((Collection<?>)compare.getValue())));
                        } else {
                            put(compare.getColumn(), new BasicDBObject("$" + compare.getCondition(), ObjectIdUtil.convertObjectId(compare.getValue())));
                        }
                    } else {
                        put(compare.getColumn(), new BasicDBObject("$" + compare.getCondition(), compare.getValue()));
                    }
                });
            }
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
            } else if (Objects.equals(compare.getColumn(), SqlOperationConstant._ID)){
                //如果是objectId
                if (ObjectId.isValid(String.valueOf(compare.getValue()))){
                    basicDBObject.put(compare.getColumn(),new BasicDBObject("$"+compare.getCondition(),new ObjectId(String.valueOf(compare.getValue()))));
                } else {
                    basicDBObject.put(compare.getColumn(),new BasicDBObject("$"+compare.getCondition(),String.valueOf(compare.getValue())));
                }
            } else {
                basicDBObject.put(compare.getColumn(), new BasicDBObject("$" + compare.getCondition(), compare.getValue()));
            }
            basicDBObjectList.add(basicDBObject);
        });
        return basicDBObjectList;
    }

    /**
     * 构建更新值
     *
     * @author JiaChaoYang
     * @date 2023/7/9 22:16
     */
    public static BasicDBObject buildUpdateValue(List<CompareCondition> compareConditionList) {
        return new BasicDBObject() {{
            compareConditionList.stream().filter(compareCondition -> compareCondition.getType() == CompareEnum.UPDATE.getKey()).collect(Collectors.toList()).forEach(compare -> {
                put(compare.getColumn(), compare.getValue());
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
    public static BasicDBObject buildGroup(List<Accumulator> accumulatorList){
        return new BasicDBObject(){{
            accumulatorList.forEach(accumulator -> {
                put(accumulator.getResultMappingField(),new BasicDBObject(){{
                    put("$"+accumulator.getCondition(),accumulator.getField());
                }});
            });
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
