package com.anwen.mongo.conditions.aggregate;

import com.anwen.mongo.conditions.accumulator.Accumulator;
import com.anwen.mongo.conditions.interfaces.aggregate.Aggregate;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.AddFields;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.ReplaceRoot;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.enums.AggregateTypeEnum;
import com.anwen.mongo.enums.GroupTypeEnum;
import com.anwen.mongo.enums.OrderEnum;
import com.anwen.mongo.enums.ProjectionEnum;
import com.anwen.mongo.execute.SqlOperation;
import com.anwen.mongo.model.BaseAggregate;
import com.anwen.mongo.strategy.aggregate.impl.*;
import com.anwen.mongo.support.SFunction;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author JiaChaoYang
 **/
@Getter
public class AggregateChainWrapper<T, Children> implements Aggregate<T, Children> {

    List<BaseAggregate> baseAggregateList = new ArrayList<>();

    protected final Children typedThis = (Children) this;

    @Override
    public Children match(QueryChainWrapper<T, ?> queryChainWrapper) {
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.MATCH.getType(), new ConcretePipelineMatch(queryChainWrapper.getCompareList())));
        return typedThis;
    }

    @Override
    public Children project(Projection... projection) {
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ConcretePipelineProject(projection)));
        return typedThis;
    }

    @Override
    public Children project(List<Projection> projectionList) {
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ConcretePipelineProject(projectionList)));
        return typedThis;
    }

    @SafeVarargs
    @Override
    public final Children projectDisplay(SFunction<T, Object>... column) {
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ConcretePipelineProject(
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
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ConcretePipelineProject(
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
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ConcretePipelineProject(
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
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ConcretePipelineProject(
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
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ConcretePipelineProject(displayId, projection)));
        return typedThis;
    }

    @SafeVarargs
    @Override
    public final Children projectDisplay(boolean displayId, SFunction<T, Object>... column) {
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ConcretePipelineProject(
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
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ConcretePipelineProject(
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
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ConcretePipelineProject(
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
        baseAggregateList.add(
                new BaseAggregate(AggregateTypeEnum.PROJECT.getType(), new ConcretePipelineProject(
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
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.SORT.getType(), new ConcretePipelineSort(orders)));
        return typedThis;
    }

    @Override
    public Children sort(List<Order> orderList) {
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.SORT.getType(), new ConcretePipelineSort(orderList)));
        return typedThis;
    }

    @SafeVarargs
    @Override
    public final Children sortAsc(SFunction<T, Object>... field) {
        baseAggregateList.add(new BaseAggregate(
                AggregateTypeEnum.SORT.getType(),
                new ConcretePipelineSort(
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
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.SORT.getType(), new ConcretePipelineSort(
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
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.SORT.getType(), new ConcretePipelineSort(
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
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.SORT.getType(), new ConcretePipelineSort(
            new ArrayList<Order>(){{
                for (String col : field) {
                    add(new Order(OrderEnum.ORDER_BY_DESC.getFlag(), col));
                }
            }}
        )));
        return typedThis;
    }

    @Override
    public Children limit(long limit) {
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.LIMIT.getType(), new ConcretePipelineLimit(limit)));
        return typedThis;
    }

    @Override
    public Children skip(long skip) {
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.SKIP.getType(), new ConcretePipelineSkip(skip)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id) {
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id.getFieldNameLine())));
        return typedThis;
    }

    @Override
    public Children group(String _id) {
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id)));
        return typedThis;
    }

    @Override
    public Children group(Accumulator... _id) {
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(new ArrayList<>(Arrays.asList(_id)))));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, Accumulator accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id.getFieldNameLine(), accumulator)));
        return typedThis;
    }

    @Override
    public Children group(String _id, Accumulator accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id, accumulator)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, Accumulator... accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id.getFieldNameLine(), accumulator)));
        return typedThis;
    }

    @Override
    public Children group(String _id, Accumulator... accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id, accumulator)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, List<Accumulator> accumulatorList) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id.getFieldNameLine(), accumulatorList)));
        return typedThis;
    }

    @Override
    public Children group(String _id, List<Accumulator> accumulatorList) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id, accumulatorList)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, String resultMappingField, String operator, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id.getFieldNameLine(), resultMappingField, operator, field)));
        return typedThis;
    }

    @Override
    public Children group(String _id, String resultMappingField, String operator, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id, resultMappingField, operator, field)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, String resultMappingField, GroupTypeEnum operator, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id.getFieldNameLine(), resultMappingField, operator.getOperator(), field)));
        return typedThis;
    }

    @Override
    public Children group(String _id, String resultMappingField, GroupTypeEnum operator, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id, resultMappingField, operator.getOperator(), field)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, SFunction<T, Object> resultMappingField, String operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id.getFieldNameLine(), resultMappingField.getFieldNameLine(), operator, field.getFieldNameLine())));
        return typedThis;
    }

    @Override
    public Children group(String _id, SFunction<T, Object> resultMappingField, String operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id, resultMappingField.getFieldNameLine(), operator, field.getFieldNameLine())));
        return typedThis;
    }

    @Override
    public Children group(String _id, SFunction<T, Object> resultMappingField, GroupTypeEnum operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id, resultMappingField.getFieldNameLine(), operator.getOperator(), field.getFieldNameLine())));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, SFunction<T, Object> resultMappingField, GroupTypeEnum operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id.getFieldNameLine(), resultMappingField.getFieldNameLine(), operator.getOperator(), field.getFieldNameLine())));
        return typedThis;
    }

    @Override
    public Children lookup() {
        return null;
    }

    @Override
    public Children addFields(String resultMappingField, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.ADD_FIELDS.getType(), new ConcretePipelineAddFields(new AddFields(resultMappingField,field.getFieldNameLine()))));
        return typedThis;
    }

    @Override
    public Children addFields(SFunction<T, Object> resultMappingField, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.ADD_FIELDS.getType(), new ConcretePipelineAddFields(new AddFields(resultMappingField.getFieldNameLine(),field.getFieldNameLine()))));
        return typedThis;
    }

    @Override
    public Children addFields(SFunction<T, Object> resultMappingField, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.ADD_FIELDS.getType(), new ConcretePipelineAddFields(new AddFields(resultMappingField.getFieldNameLine(),field))));
        return typedThis;
    }

    @Override
    public Children addFields(String resultMappingField, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.ADD_FIELDS.getType(), new ConcretePipelineAddFields(new AddFields(resultMappingField,field))));
        return typedThis;
    }

    @Override
    public Children addFields(AddFields... addFields) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.ADD_FIELDS.getType(), new ConcretePipelineAddFields(addFields)));
        return typedThis;
    }

    @Override
    public Children addFields(List<AddFields> addFieldsList) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.ADD_FIELDS.getType(), new ConcretePipelineAddFields(addFieldsList)));
        return typedThis;
    }

    @Override
    public Children unwind(SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.UNWIND.getType(), new ConcretePipelineUnwind(field.getFieldNameLine(),false)));
        return typedThis;
    }

    @Override
    public Children unwind(Boolean preserveNullAndEmptyArrays, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.UNWIND.getType(), new ConcretePipelineUnwind(field.getFieldNameLine(),preserveNullAndEmptyArrays)));
        return typedThis;
    }

    @Override
    public Children unwind(String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.UNWIND.getType(), new ConcretePipelineUnwind(field,false)));
        return typedThis;
    }

    @Override
    public Children unwind(Boolean preserveNullAndEmptyArrays, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.UNWIND.getType(), new ConcretePipelineUnwind(field,preserveNullAndEmptyArrays)));
        return typedThis;
    }

    @Override
    public Children sample(long size) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.SAMPLE.getType(), new ConcretePipelineSample(size)));
        return typedThis;
    }

    @SafeVarargs
    @Override
    public final Children replaceRoot(SFunction<T, Object>... field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.REPLACE_ROOT.getType(), new ConcretePipelineReplaceRoot(false,field)));
        return typedThis;
    }

    @Override
    public Children replaceRoot(ReplaceRoot... replaceRoot) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.REPLACE_ROOT.getType(), new ConcretePipelineReplaceRoot(false,replaceRoot)));
        return typedThis;
    }

    @Override
    public Children replaceRoot(List<ReplaceRoot> replaceRootList) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.REPLACE_ROOT.getType(), new ConcretePipelineReplaceRoot(false,replaceRootList)));
        return typedThis;
    }

    @Override
    public Children replaceRoot(String... field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.REPLACE_ROOT.getType(), new ConcretePipelineReplaceRoot(false,field)));
        return typedThis;
    }

    @SafeVarargs
    @Override
    public final Children replaceRoot(Boolean reserveOriginalDocument, SFunction<T, Object>... field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.REPLACE_ROOT.getType(), new ConcretePipelineReplaceRoot(reserveOriginalDocument,field)));
        return typedThis;
    }

    @Override
    public Children replaceRoot(Boolean reserveOriginalDocument, String... field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.REPLACE_ROOT.getType(), new ConcretePipelineReplaceRoot(reserveOriginalDocument,field)));
        return typedThis;
    }

    @Override
    public Children out(String coll) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.OUT.getType(), new ConcretePipelineOut(null,coll)));
        return typedThis;
    }

    @Override
    public Children out(String db, String coll) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.OUT.getType(), new ConcretePipelineOut(db,coll)));
        return typedThis;
    }
}
