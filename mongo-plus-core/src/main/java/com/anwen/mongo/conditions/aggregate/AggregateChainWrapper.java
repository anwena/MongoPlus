package com.anwen.mongo.conditions.aggregate;

import com.anwen.mongo.conditions.accumulator.Accumulator;
import com.anwen.mongo.conditions.interfaces.aggregate.Aggregate;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.AddFields;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Let;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.ReplaceRoot;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.enums.*;
import com.anwen.mongo.model.BaseAggregate;
import com.anwen.mongo.model.FuncGroupField;
import com.anwen.mongo.model.GroupField;
import com.anwen.mongo.strategy.aggregate.impl.*;
import com.anwen.mongo.support.SFunction;
import com.mongodb.BasicDBObject;
import com.mongodb.client.model.Collation;
import com.mongodb.client.model.CollationStrength;
import org.bson.BsonValue;
import org.bson.conversions.Bson;

import java.util.*;

/**
 * @author JiaChaoYang
 **/
public class AggregateChainWrapper<T, Children> implements Aggregate<T, Children> {

    List<BaseAggregate> baseAggregateList = new ArrayList<>();

    List<BasicDBObject> basicDBObjectList = new ArrayList<>();

    BasicDBObject optionsBasicDBObject = new BasicDBObject();

    Map<String,Integer> orderMap = new HashMap<>();

    protected final Children typedThis = (Children) this;

    @Override
    public Children match(QueryChainWrapper<?, ?> queryChainWrapper) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.MATCH.getType(), new MatchConcretePipeline(queryChainWrapper.getCompareList(),queryChainWrapper.getBasicDBObjectList())));
        return typedThis;
    }

    @Override
    public Children match(BasicDBObject basicDBObject) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.MATCH.getType(), new DefaultConcretePipeline(basicDBObject)));
        return typedThis;
    }

    @Override
    public Children match(Bson bson) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.MATCH.getType(), new DefaultConcretePipeline(BasicDBObject.parse(bson.toBsonDocument().toJson()))));
        return typedThis;
    }

    @Override
    public Children project(Projection... projection) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ProjectConcretePipeline(projection)));
        return typedThis;
    }

    @Override
    public Children project(List<Projection> projectionList) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ProjectConcretePipeline(projectionList)));
        return typedThis;
    }

    @SafeVarargs
    @Override
    public final Children projectDisplay(SFunction<T, Object>... column) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ProjectConcretePipeline(
                new ArrayList<Projection>() {{
                    for (SFunction<T, Object> sFunction : column) {
                        add(new Projection(sFunction.getFieldNameLine(), ProjectionEnum.DISPLAY.getValue()));
                    }
                }}
        )));
        return typedThis;
    }

    @Override
    public Children projectDisplay(String... column) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ProjectConcretePipeline(
                new ArrayList<Projection>() {{
                    for (String col : column) {
                        add(new Projection(col, ProjectionEnum.DISPLAY.getValue()));
                    }
                }}
        )));
        return typedThis;
    }

    @SafeVarargs
    @Override
    public final Children projectNone(SFunction<T, Object>... column) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ProjectConcretePipeline(
                new ArrayList<Projection>() {{
                    for (SFunction<T, Object> sFunction : column) {
                        add(new Projection(sFunction.getFieldNameLine(), ProjectionEnum.NONE.getValue()));
                    }
                }}
        )));
        return typedThis;
    }

    @Override
    public Children projectNone(String... column) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ProjectConcretePipeline(
                new ArrayList<Projection>() {{
                    for (String col : column) {
                        add(new Projection(col, ProjectionEnum.NONE.getValue()));
                    }
                }}
        )));
        return typedThis;
    }

    @Override
    public Children project(boolean displayId, Projection... projection) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ProjectConcretePipeline(displayId, projection)));
        return typedThis;
    }

    @SafeVarargs
    @Override
    public final Children projectDisplay(boolean displayId, SFunction<T, Object>... column) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ProjectConcretePipeline(
                new ArrayList<Projection>() {{
                    for (SFunction<T, Object> sFunction : column) {
                        add(new Projection(sFunction.getFieldNameLine(), ProjectionEnum.DISPLAY.getValue()));
                    }
                    if (!displayId) {
                        add(new Projection(SqlOperationConstant._ID, ProjectionEnum.NONE.getValue()));
                    }
                }}
        )));
        return typedThis;
    }

    @Override
    public Children projectDisplay(boolean displayId, String... column) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ProjectConcretePipeline(
                new ArrayList<Projection>() {{
                    for (String col : column) {
                        add(new Projection(col, ProjectionEnum.DISPLAY.getValue()));
                    }
                    if (!displayId) {
                        add(new Projection(SqlOperationConstant._ID, ProjectionEnum.NONE.getValue()));
                    }
                }}
        )));
        return typedThis;
    }

    @SafeVarargs
    @Override
    public final Children projectNone(boolean displayId, SFunction<T, Object>... column) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ProjectConcretePipeline(
                new ArrayList<Projection>() {{
                    for (SFunction<T, Object> sFunction : column) {
                        add(new Projection(sFunction.getFieldNameLine(), ProjectionEnum.NONE.getValue()));
                    }
                    if (!displayId) {
                        add(new Projection(SqlOperationConstant._ID, ProjectionEnum.NONE.getValue()));
                    }
                }}
        )));
        return typedThis;
    }

    @Override
    public Children projectNone(boolean displayId, String... column) {
        this.baseAggregateList.add(
                new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ProjectConcretePipeline(
                        new ArrayList<Projection>() {{
                            for (String col : column) {
                                add(new Projection(col, ProjectionEnum.NONE.getValue()));
                            }
                            if (!displayId) {
                                add(new Projection(SqlOperationConstant._ID, ProjectionEnum.NONE.getValue()));
                            }
                        }})
                )
        );
        return typedThis;
    }


    @Override
    public Children project(BasicDBObject basicDBObject) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new DefaultConcretePipeline(basicDBObject)));
        return typedThis;
    }

    @Override
    public Children project(Bson bson) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new DefaultConcretePipeline(BasicDBObject.parse(bson.toBsonDocument().toJson()))));
        return typedThis;
    }

    @Override
    public Children sort(Order... orders) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.SORT.getType(), new SortConcretePipeline(orders)));
        return typedThis;
    }

    @Override
    public Children sort(List<Order> orderList) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.SORT.getType(), new SortConcretePipeline(orderList)));
        return typedThis;
    }

    @SafeVarargs
    @Override
    public final Children sortAsc(SFunction<T, Object>... field) {
        this.baseAggregateList.add(new BaseAggregate(
                AggregateTypeEnum.SORT.getType(),
                new SortConcretePipeline(
                        new ArrayList<Order>() {{
                            for (SFunction<T, Object> sFunction : field) {
                                add(new Order(OrderEnum.ORDER_BY.getFlag(), sFunction.getFieldNameLine()));
                            }
                        }}
                )
        ));
        return typedThis;
    }

    @Override
    public Children sortAsc(String... field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.SORT.getType(), new SortConcretePipeline(
            new ArrayList<Order>(){{
                for (String col : field) {
                    add(new Order(OrderEnum.ORDER_BY.getFlag(), col));
                }
            }}
        )));
        return typedThis;
    }

    @SafeVarargs
    @Override
    public final Children sortDesc(SFunction<T, Object>... field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.SORT.getType(), new SortConcretePipeline(
                new ArrayList<Order>(){{
                    for (SFunction<T, Object> sFunction : field) {
                        add(new Order(OrderEnum.ORDER_BY_DESC.getFlag(),sFunction.getFieldNameLine()));
                    }
                }}
        )));
        return typedThis;
    }

    @Override
    public Children sortDesc(String... field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.SORT.getType(), new SortConcretePipeline(
            new ArrayList<Order>(){{
                for (String col : field) {
                    add(new Order(OrderEnum.ORDER_BY_DESC.getFlag(), col));
                }
            }}
        )));
        return typedThis;
    }

    @Override
    public Children sort(BasicDBObject basicDBObject) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.SORT.getType(), new DefaultConcretePipeline(basicDBObject)));
        return typedThis;
    }

    @Override
    public Children sort(Bson bson) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.SORT.getType(), new DefaultConcretePipeline(BasicDBObject.parse(bson.toBsonDocument().toJson()))));
        return typedThis;
    }

    @Override
    public Children limit(long limit) {
        this.basicDBObjectList.add(new BasicDBObject(AggregateTypeEnum.LIMIT.getType(),limit));
        return typedThis;
    }

    @Override
    public Children skip(long skip) {
        this.basicDBObjectList.add(new BasicDBObject(AggregateTypeEnum.SKIP.getType(),skip));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id.getFieldNameLine())));
        return typedThis;
    }

    @Override
    public Children groupFunc(List<FuncGroupField<?>> _id) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(new ArrayList<GroupField>(){{
            _id.forEach(funcGroupField -> {
                add(new GroupField(funcGroupField.getGroupField(),funcGroupField.getField()));
            });
        }},true)));
        return typedThis;
    }

    @Override
    public Children group(String _id) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id)));
        return typedThis;
    }

    @Override
    public Children group(List<GroupField> _id) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id,true)));
        return typedThis;
    }

    @Override
    public Children group(Accumulator... _id) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(new ArrayList<>(Arrays.asList(_id)))));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, Accumulator accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id.getFieldNameLine(), accumulator)));
        return typedThis;
    }

    @Override
    public Children groupFunc(List<FuncGroupField<?>> _id, Accumulator accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(new ArrayList<GroupField>(){{
            _id.forEach(funcGroupField -> {
                add(new GroupField(funcGroupField.getGroupField(),funcGroupField.getField()));
            });
        }}, accumulator,true)));
        return typedThis;
    }

    @Override
    public Children group(String _id, Accumulator accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, accumulator)));
        return typedThis;
    }

    @Override
    public Children group(List<GroupField> _id, Accumulator accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, accumulator,true)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, Accumulator... accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id.getFieldNameLine(), accumulator)));
        return typedThis;
    }

    @Override
    public Children groupFunc(List<FuncGroupField<?>> _id, Accumulator... accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(new ArrayList<GroupField>(){{
            _id.forEach(funcGroupField -> {
                add(new GroupField(funcGroupField.getGroupField(),funcGroupField.getField()));
            });
        }}, true,accumulator)));
        return typedThis;
    }

    @Override
    public Children group(String _id, Accumulator... accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, accumulator)));
        return typedThis;
    }

    @Override
    public Children group(List<GroupField> _id, Accumulator... accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, true,accumulator)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, List<Accumulator> accumulatorList) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id.getFieldNameLine(), accumulatorList)));
        return typedThis;
    }

    @Override
    public Children groupFunc(List<FuncGroupField<?>> _id, List<Accumulator> accumulatorList) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(new ArrayList<GroupField>(){{
            _id.forEach(funcGroupField -> {
                add(new GroupField(funcGroupField.getGroupField(),funcGroupField.getField()));
            });
        }}, accumulatorList)));
        return typedThis;
    }

    @Override
    public Children group(String _id, List<Accumulator> accumulatorList) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, accumulatorList)));
        return typedThis;
    }

    @Override
    public Children group(List<GroupField> _id, List<Accumulator> accumulatorList) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, accumulatorList)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, String resultMappingField, String operator, Object field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id.getFieldNameLine(), resultMappingField, operator, field)));
        return typedThis;
    }

    @Override
    public Children groupFunc(List<FuncGroupField<?>> _id, String resultMappingField, String operator, Object field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(new ArrayList<GroupField>(){{
            _id.forEach(funcGroupField -> {
                add(new GroupField(funcGroupField.getGroupField(),funcGroupField.getField()));
            });
        }}, resultMappingField, operator, field,true)));
        return typedThis;
    }

    @Override
    public Children group(String _id, String resultMappingField, String operator, Object field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, resultMappingField, operator, field)));
        return typedThis;
    }

    @Override
    public Children group(List<GroupField> _id, String resultMappingField, String operator, Object field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, resultMappingField, operator, field,true)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, String resultMappingField, GroupTypeEnum operator, Object field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id.getFieldNameLine(), resultMappingField, operator.getOperator(), field)));
        return typedThis;
    }

    @Override
    public Children groupFunc(List<FuncGroupField<?>> _id, String resultMappingField, GroupTypeEnum operator, Object field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(new ArrayList<GroupField>(){{
            _id.forEach(funcGroupField -> {
                add(new GroupField(funcGroupField.getGroupField(),funcGroupField.getField()));
            });
        }}, resultMappingField, operator.getOperator(), field,true)));
        return typedThis;
    }

    @Override
    public Children group(String _id, String resultMappingField, GroupTypeEnum operator, Object field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, resultMappingField, operator.getOperator(), field)));
        return typedThis;
    }

    @Override
    public Children group(List<GroupField> _id, String resultMappingField, GroupTypeEnum operator, Object field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, resultMappingField, operator.getOperator(), field,true)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, SFunction<T, Object> resultMappingField, String operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id.getFieldNameLine(), resultMappingField.getFieldNameLine(), operator, field.getFieldNameLine())));
        return typedThis;
    }

    @Override
    public Children groupFunc(List<FuncGroupField<?>> _id, SFunction<T, Object> resultMappingField, String operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(new ArrayList<GroupField>(){{
            _id.forEach(funcGroupField -> {
                add(new GroupField(funcGroupField.getGroupField(),funcGroupField.getField()));
            });
        }}, resultMappingField.getFieldNameLine(), operator, field.getFieldNameLine(),true)));
        return typedThis;
    }

    @Override
    public Children group(String _id, SFunction<T, Object> resultMappingField, String operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, resultMappingField.getFieldNameLine(), operator, field.getFieldNameLine())));
        return typedThis;
    }

    @Override
    public Children group(List<GroupField> _id, SFunction<T, Object> resultMappingField, String operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, resultMappingField.getFieldNameLine(), operator, field.getFieldNameLine(),true)));
        return typedThis;
    }

    @Override
    public Children group(String _id, SFunction<T, Object> resultMappingField, GroupTypeEnum operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, resultMappingField.getFieldNameLine(), operator.getOperator(), field.getFieldNameLine())));
        return typedThis;
    }

    @Override
    public Children group(List<GroupField> _id, SFunction<T, Object> resultMappingField, GroupTypeEnum operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, resultMappingField.getFieldNameLine(), operator.getOperator(), field.getFieldNameLine(),true)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, SFunction<T, Object> resultMappingField, GroupTypeEnum operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id.getFieldNameLine(), resultMappingField.getFieldNameLine(), operator.getOperator(), field.getFieldNameLine())));
        return typedThis;
    }

    @Override
    public Children groupFunc(List<FuncGroupField<?>> _id, SFunction<T, Object> resultMappingField, GroupTypeEnum operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(new ArrayList<GroupField>(){{
            _id.forEach(funcGroupField -> {
                add(new GroupField(funcGroupField.getGroupField(),funcGroupField.getField()));
            });
        }}, resultMappingField.getFieldNameLine(), operator.getOperator(), field.getFieldNameLine(),true)));
        return typedThis;
    }

    @Override
    public Children group(BasicDBObject basicDBObject) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new DefaultConcretePipeline(basicDBObject)));
        return typedThis;
    }

    @Override
    public Children group(Bson bson) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new DefaultConcretePipeline(BasicDBObject.parse(bson.toBsonDocument().toJson()))));
        return typedThis;
    }

    @Override
    public Children lookup(String from, String localField, String foreignField, String as) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.LOOKUP.getType(), new LookupConcretePipeline(new BasicDBObject(){{
            put("from",from);
            put("localField",localField);
            put("foreignField",foreignField);
            put("as",as);
        }})));
        return typedThis;
    }

    @Override
    public Children lookup(String from, List<Let> letList, AggregateChainWrapper<T, ?> pipeline, String as) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.LOOKUP.getType(), new LookupConcretePipeline(new BasicDBObject(){{
            put("from",from);
            put("pipeline",new ArrayList<BasicDBObject>(){{
                pipeline.getBaseAggregateList().forEach(baseAggregate -> add(new BasicDBObject("$"+baseAggregate.getType(),baseAggregate.getPipelineStrategy().buildAggregate())));
                addAll(pipeline.getBasicDBObjectList());
            }});
            put("as",as);
        }})));
        return typedThis;
    }

    @Override
    public Children lookup(BasicDBObject basicDBObject) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.LOOKUP.getType(), new DefaultConcretePipeline(basicDBObject)));
        return typedThis;
    }

    @Override
    public Children lookup(Bson bson) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.LOOKUP.getType(), new DefaultConcretePipeline(BasicDBObject.parse(bson.toBsonDocument().toJson()))));
        return typedThis;
    }

    @Override
    public Children addFields(String resultMappingField, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.ADD_FIELDS.getType(), new AddFieldsConcretePipeline(new AddFields(resultMappingField,field.getFieldNameLine()))));
        return typedThis;
    }

    @Override
    public Children addFields(SFunction<T, Object> resultMappingField, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.ADD_FIELDS.getType(), new AddFieldsConcretePipeline(new AddFields(resultMappingField.getFieldNameLine(),field.getFieldNameLine()))));
        return typedThis;
    }

    @Override
    public Children addFields(SFunction<T, Object> resultMappingField, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.ADD_FIELDS.getType(), new AddFieldsConcretePipeline(new AddFields(resultMappingField.getFieldNameLine(),field))));
        return typedThis;
    }

    @Override
    public Children addFields(String resultMappingField, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.ADD_FIELDS.getType(), new AddFieldsConcretePipeline(new AddFields(resultMappingField,field))));
        return typedThis;
    }

    @Override
    public Children addFields(AddFields... addFields) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.ADD_FIELDS.getType(), new AddFieldsConcretePipeline(addFields)));
        return typedThis;
    }

    @Override
    public Children addFields(List<AddFields> addFieldsList) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.ADD_FIELDS.getType(), new AddFieldsConcretePipeline(addFieldsList)));
        return typedThis;
    }

    @Override
    public Children addFields(BasicDBObject basicDBObject) {
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.ADD_FIELDS.getType(), new DefaultConcretePipeline(basicDBObject)));
        return typedThis;
    }

    @Override
    public Children addFields(Bson bson) {
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.ADD_FIELDS.getType(), new DefaultConcretePipeline(BasicDBObject.parse(bson.toBsonDocument().toJson()))));
        return typedThis;
    }

    @Override
    public Children unwind(SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.UNWIND.getType(), new UnwindConcretePipeline(field.getFieldNameLine(),false)));
        return typedThis;
    }

    @Override
    public Children unwind(Boolean preserveNullAndEmptyArrays, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.UNWIND.getType(), new UnwindConcretePipeline(field.getFieldNameLine(),preserveNullAndEmptyArrays)));
        return typedThis;
    }

    @Override
    public Children unwind(String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.UNWIND.getType(), new UnwindConcretePipeline(field,false)));
        return typedThis;
    }

    @Override
    public Children unwind(Boolean preserveNullAndEmptyArrays, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.UNWIND.getType(), new UnwindConcretePipeline(field,preserveNullAndEmptyArrays)));
        return typedThis;
    }

    @Override
    public Children unwind(BasicDBObject basicDBObject) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.UNWIND.getType(), new DefaultConcretePipeline(basicDBObject)));
        return typedThis;
    }

    @Override
    public Children unwind(Bson bson) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.UNWIND.getType(), new DefaultConcretePipeline(BasicDBObject.parse(bson.toBsonDocument().toJson()))));
        return typedThis;
    }

    @Override
    public Children sample(long size) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.SAMPLE.getType(), new SampleConcretePipeline(size)));
        return typedThis;
    }

    @SafeVarargs
    @Override
    public final Children replaceRoot(SFunction<T, Object>... field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.REPLACE_ROOT.getType(), new RootConcretePipelineReplace(false,field)));
        return typedThis;
    }

    @Override
    public Children replaceRoot(ReplaceRoot... replaceRoot) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.REPLACE_ROOT.getType(), new RootConcretePipelineReplace(false,replaceRoot)));
        return typedThis;
    }

    @Override
    public Children replaceRoot(List<ReplaceRoot> replaceRootList) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.REPLACE_ROOT.getType(), new RootConcretePipelineReplace(false,replaceRootList)));
        return typedThis;
    }

    @Override
    public Children replaceRoot(String... field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.REPLACE_ROOT.getType(), new RootConcretePipelineReplace(false,field)));
        return typedThis;
    }

    @SafeVarargs
    @Override
    public final Children replaceRoot(Boolean reserveOriginalDocument, SFunction<T, Object>... field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.REPLACE_ROOT.getType(), new RootConcretePipelineReplace(reserveOriginalDocument,field)));
        return typedThis;
    }

    @Override
    public Children replaceRoot(Boolean reserveOriginalDocument, String... field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.REPLACE_ROOT.getType(), new RootConcretePipelineReplace(reserveOriginalDocument,field)));
        return typedThis;
    }

    @Override
    public Children replaceRoot(BasicDBObject basicDBObject) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.REPLACE_ROOT.getType(), new DefaultConcretePipeline(basicDBObject)));
        return typedThis;
    }

    @Override
    public Children replaceRoot(Bson bson) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.REPLACE_ROOT.getType(), new DefaultConcretePipeline(BasicDBObject.parse(bson.toBsonDocument().toJson()))));
        return typedThis;
    }

    @Override
    public Children unionWith(String collectionName) {
        this.basicDBObjectList.add(new BasicDBObject(AggregateTypeEnum.UNION_WITH.getType(),collectionName));
        return typedThis;
    }

    @Override
    public Children unionWith(BasicDBObject basicDBObject) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.UNWIND.getType(), new DefaultConcretePipeline(basicDBObject)));
        return typedThis;
    }

    @Override
    public Children unionWith(Bson bson) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.UNWIND.getType(), new DefaultConcretePipeline(BasicDBObject.parse(bson.toBsonDocument().toJson()))));
        return typedThis;
    }

    @Override
    public Children out(String coll) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.OUT.getType(), new OutConcretePipeline(null,coll)));
        return typedThis;
    }

    @Override
    public Children out(String db, String coll) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.OUT.getType(), new OutConcretePipeline(db,coll)));
        return typedThis;
    }

    @Override
    public Children out(BasicDBObject basicDBObject) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.OUT.getType(), new DefaultConcretePipeline(basicDBObject)));
        return typedThis;
    }

    @Override
    public Children out(Bson bson) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.OUT.getType(), new DefaultConcretePipeline(BasicDBObject.parse(bson.toBsonDocument().toJson()))));
        return typedThis;
    }

    @Override
    public Children custom(BasicDBObject basicDBObject) {
        this.basicDBObjectList.add(basicDBObject);
        return typedThis;
    }

    @Override
    public Children custom(Bson bson) {
        this.basicDBObjectList.add(BasicDBObject.parse(bson.toBsonDocument().toJson()));
        return typedThis;
    }

    @Override
    public Children allowDiskUse(boolean allowDiskUse) {
        this.optionsBasicDBObject.append(AggregateOptionsEnum.ALLOW_DISK_USE.getOptions(), allowDiskUse);
        return typedThis;
    }

    @Override
    public Children batchSize(Integer size) {
        this.optionsBasicDBObject.append(AggregateOptionsEnum.BATCH_SIZE.getOptions(), size);
        return typedThis;
    }

    @Override
    public Children collation(CollationStrength collationStrength) {
        this.optionsBasicDBObject.append(AggregateOptionsEnum.COLLATION.getOptions(), Collation.builder().collationStrength(collationStrength).build().asDocument());
        return typedThis;
    }

    @Override
    public Children maxTimeMS(long time) {
        this.optionsBasicDBObject.append(AggregateOptionsEnum.MAX_TIME_MS.getOptions(), time);
        return typedThis;
    }

    @Override
    public Children maxAwaitTimeMS(long maxAwaitTime) {
        this.optionsBasicDBObject.append(AggregateOptionsEnum.MAX_AWAIT_TIME_MS.getOptions(), maxAwaitTime);
        return typedThis;
    }

    @Override
    public Children bypassDocumentValidation(boolean bypassDocumentValidation) {
        this.optionsBasicDBObject.append(AggregateOptionsEnum.BYPASS_DOCUMENT_VALIDATION.getOptions(), bypassDocumentValidation);
        return typedThis;
    }

    @Override
    public Children comment(BsonValue comment) {
        this.optionsBasicDBObject.append(AggregateOptionsEnum.COMMENT.getOptions(), comment);
        return typedThis;
    }

    @Override
    public Children comment(String comment) {
        this.optionsBasicDBObject.append(AggregateOptionsEnum.COMMENT_STR.getOptions(), comment);
        return typedThis;
    }

    @Override
    public Children hint(Bson hint) {
        this.optionsBasicDBObject.append(AggregateOptionsEnum.HINT.getOptions(), hint);
        return typedThis;
    }

    @Override
    public Children hint(String hint) {
        this.optionsBasicDBObject.append(AggregateOptionsEnum.HINT_STR.getOptions(), hint);
        return typedThis;
    }

    @Override
    public Children let(Bson variables) {
        this.optionsBasicDBObject.append(AggregateOptionsEnum.LET.getOptions(), variables);
        return typedThis;
    }

    public List<BaseAggregate> getBaseAggregateList() {
        return baseAggregateList;
    }

    public List<BasicDBObject> getBasicDBObjectList() {
        return basicDBObjectList;
    }

    public BasicDBObject getOptionsBasicDBObject() {
        return optionsBasicDBObject;
    }
}
