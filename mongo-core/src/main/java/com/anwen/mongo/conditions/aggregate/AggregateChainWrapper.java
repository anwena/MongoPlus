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
import com.anwen.mongo.enums.AggregateTypeEnum;
import com.anwen.mongo.enums.GroupTypeEnum;
import com.anwen.mongo.enums.OrderEnum;
import com.anwen.mongo.enums.ProjectionEnum;
import com.anwen.mongo.model.BaseAggregate;
import com.anwen.mongo.strategy.aggregate.impl.*;
import com.anwen.mongo.support.SFunction;
import com.mongodb.BasicDBObject;
import lombok.Getter;
import org.bson.conversions.Bson;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author JiaChaoYang
 **/
@Getter
public class AggregateChainWrapper<T, Children> implements Aggregate<T, Children> {

    List<BaseAggregate> baseAggregateList = new ArrayList<>();

    List<BasicDBObject> basicDBObjectList = new ArrayList<>();

    protected final Children typedThis = (Children) this;

    @Override
    public Children match(QueryChainWrapper<T, ?> queryChainWrapper) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.MATCH.getType(), new MatchConcretePipeline(queryChainWrapper.getCompareList())));
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
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.LIMIT.getType(), new LimitConcretePipeline(limit)));
        return typedThis;
    }

    @Override
    public Children skip(long skip) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.SKIP.getType(), new SkipConcretePipeline(skip)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id.getFieldNameLine())));
        return typedThis;
    }

    @Override
    public Children group(String _id) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id)));
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
    public Children group(String _id, Accumulator accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, accumulator)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, Accumulator... accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id.getFieldNameLine(), accumulator)));
        return typedThis;
    }

    @Override
    public Children group(String _id, Accumulator... accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, accumulator)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, List<Accumulator> accumulatorList) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id.getFieldNameLine(), accumulatorList)));
        return typedThis;
    }

    @Override
    public Children group(String _id, List<Accumulator> accumulatorList) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, accumulatorList)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, String resultMappingField, String operator, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id.getFieldNameLine(), resultMappingField, operator, field)));
        return typedThis;
    }

    @Override
    public Children group(String _id, String resultMappingField, String operator, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, resultMappingField, operator, field)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, String resultMappingField, GroupTypeEnum operator, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id.getFieldNameLine(), resultMappingField, operator.getOperator(), field)));
        return typedThis;
    }

    @Override
    public Children group(String _id, String resultMappingField, GroupTypeEnum operator, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, resultMappingField, operator.getOperator(), field)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, SFunction<T, Object> resultMappingField, String operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id.getFieldNameLine(), resultMappingField.getFieldNameLine(), operator, field.getFieldNameLine())));
        return typedThis;
    }

    @Override
    public Children group(String _id, SFunction<T, Object> resultMappingField, String operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, resultMappingField.getFieldNameLine(), operator, field.getFieldNameLine())));
        return typedThis;
    }

    @Override
    public Children group(String _id, SFunction<T, Object> resultMappingField, GroupTypeEnum operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id, resultMappingField.getFieldNameLine(), operator.getOperator(), field.getFieldNameLine())));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, SFunction<T, Object> resultMappingField, GroupTypeEnum operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new GroupConcretePipeline(_id.getFieldNameLine(), resultMappingField.getFieldNameLine(), operator.getOperator(), field.getFieldNameLine())));
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
                pipeline.getBaseAggregateList().forEach(baseAggregate -> {
                    add(new BasicDBObject("$"+baseAggregate.getType(),baseAggregate.getPipelineStrategy().buildAggregate()));
                });
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
}
