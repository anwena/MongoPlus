package com.anwen.mongo.sql.conditions;

import com.anwen.mongo.enums.CpmpareEnum;
import com.anwen.mongo.enums.LogicTypeEnum;
import com.anwen.mongo.enums.TypeEnum;
import com.anwen.mongo.sql.conditions.interfaces.Compare;
import com.anwen.mongo.enums.OrderEnum;
import com.anwen.mongo.sql.interfaces.CompareCondition;
import com.anwen.mongo.sql.interfaces.Order;
import com.anwen.mongo.sql.query.LambdaQueryChainWrapper;
import com.anwen.mongo.sql.support.SFunction;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * 查询条件
 * @author JiaChaoYang
 * @date 2023/6/24/024 0:49
*/
@Getter
public class AbstractChainWrapper<T, Children extends AbstractChainWrapper<T, Children>> implements Compare<Children,T> {

    protected final Children typedThis = (Children) this;

    /**
     * 构建条件对象
     * @since 2023/2/10 12:00
     */
    List<CompareCondition> compareConditionList = new ArrayList<>();

    /**
     * 构建排序对象
     * @since 2023/2/10 12:00
     */
    List<Order> orderList = new ArrayList<>();

    /**
     * 查询条件
     * @author JiaChaoYang
     * @date 2023/6/25/025 15:17
    */
    public List<CompareCondition> getCompareList() {
        return compareConditionList;
    }

    @Override
    public Children eq(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? eq(column,value) : typedThis;
    }

    @Override
    public Children eq(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children eq(boolean condition, String column, Object value) {
        return condition ? eq(column,value) : typedThis;
    }

    @Override
    public Children eq(String column, Object value) {
        return getBaseCondition(column,value,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children ne(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? ne(column,value) : typedThis;
    }

    @Override
    public Children ne(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children ne(boolean condition, String column, Object value) {
        return condition ? ne(column,value) : typedThis;
    }

    @Override
    public Children ne(String column, Object value) {
        return getBaseCondition(column,value,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children lt(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? lt(column,value) : typedThis;
    }

    @Override
    public Children lt(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children lt(boolean condition, String column, Object value) {
        return condition ? lt(column,value) : typedThis;
    }

    @Override
    public Children lt(String column, Object value) {
        return getBaseCondition(column,value,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children lte(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? lte(column,value) : typedThis;
    }

    @Override
    public Children lte(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children lte(boolean condition, String column, Object value) {
        return condition ? lt(column,value) : typedThis;
    }

    @Override
    public Children lte(String column, Object value) {
        return getBaseCondition(column,value,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children gt(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? gt(column,value) : typedThis;
    }

    @Override
    public Children gt(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children gt(boolean condition, String column, Object value) {
        return condition ? gt(column,value) : typedThis;
    }

    @Override
    public Children gt(String column, Object value) {
        return getBaseCondition(column,value,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children gte(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? gte(column,value) : typedThis;
    }

    @Override
    public Children gte(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children gte(boolean condition, String column, Object value) {
        return condition ? gte(column,value) : typedThis;
    }

    @Override
    public Children gte(String column, Object value) {
        return getBaseCondition(column,value,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children like(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? like(column,value) : typedThis;
    }

    @Override
    public Children like(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children like(boolean condition, String column, Object value) {
        return condition ? like(column,value) : typedThis;
    }

    @Override
    public Children like(String column, Object value) {
        return getBaseCondition(column,value,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children in(boolean condition, SFunction<T, Object> column, Collection<Object> valueList) {
        return condition ? in(column,valueList) : typedThis;
    }

    @Override
    public Children in(SFunction<T, Object> column, Collection<Object> valueList) {
        return getBaseCondition(column,valueList,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children in(boolean condition, String column, Collection<Object> valueList) {
        return condition ? in(column,valueList) : typedThis;
    }

    @Override
    public Children in(String column, Collection<Object> valueList) {
        return getBaseCondition(column,valueList,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children nin(boolean condition, SFunction<T, Object> column, Collection<Object> valueList) {
        return condition ? nin(column,valueList) : typedThis;
    }

    @Override
    public Children nin(SFunction<T, Object> column, Collection<Object> valueList) {
        return getBaseCondition(column,valueList,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children nin(boolean condition, String column, Collection<Object> valueList) {
        return condition ? nin(column,valueList) : typedThis;
    }

    @Override
    public Children nin(String column, Collection<Object> valueList) {
        return getBaseCondition(column,valueList,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children type(SFunction<T, Object> column, TypeEnum value) {
        return getBaseCondition(column,value.getTypeCode(),LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children type(String column, TypeEnum value) {
        return getBaseCondition(column,value.getTypeCode(),LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children type(SFunction<T, Object> column, String value) {
        return getBaseCondition(column,value,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children type(String column, String value) {
        return getBaseCondition(column,value,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children type(SFunction<T, Object> column, Integer value) {
        return getBaseCondition(column,value,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children type(String column, Integer value) {
        return getBaseCondition(column,value,LogicTypeEnum.AND.getKey());
    }

    @Override
    public Children orderByAsc(SFunction<T, Object> column) {
        return getBaseOrder(OrderEnum.ORDER_BY.getFlag(), column);
    }

    @Override
    public Children orderByDesc(SFunction<T, Object> column) {
        return getBaseOrder(OrderEnum.ORDER_BY_DESC.getFlag(), column);
    }

    @Override
    public Children orderByAsc(String column) {
        return getBaseOrder(OrderEnum.ORDER_BY.getFlag(), column);
    }

    @Override
    public Children orderByDesc(String column) {
        return getBaseOrder(OrderEnum.ORDER_BY_DESC.getFlag(), column);
    }

    public Children getBaseCondition(String column, Object value,Integer logic){
        compareConditionList.add(new CompareCondition(new Throwable().getStackTrace()[1].getMethodName(), column,value, CpmpareEnum.QUERY.getKey(), logic));
        return typedThis;
    }

    public Children getBaseOrCondition(List<CompareCondition> compareConditionList){
        this.compareConditionList.add(new CompareCondition(CpmpareEnum.QUERY.getKey(),LogicTypeEnum.OR.getKey(), compareConditionList));
        return typedThis;
    }

    public Children getBaseAndCondition(List<CompareCondition> compareConditionList){
        CompareCondition compareCondition = new CompareCondition();
        compareCondition.setCondition("and");
        compareCondition.setType(CpmpareEnum.QUERY.getKey());
        compareCondition.setLogicType(LogicTypeEnum.AND.getKey());
        compareCondition.setChildCondition(compareConditionList);
        this.compareConditionList.add(compareCondition);
        return typedThis;
    }

    public Children getBaseCondition(SFunction<T, Object> column, Object value,Integer logic){
        compareConditionList.add(new CompareCondition(new Throwable().getStackTrace()[1].getMethodName(), column.getFieldNameLine(),value,CpmpareEnum.QUERY.getKey(),logic));
        return typedThis;
    }

    public Children getBaseOrder(Integer type , String column){
        orderList.add(new Order(type,column));
        return typedThis;
    }

    public Children getBaseOrder(Integer type , SFunction<T, Object> column){
        orderList.add(new Order(type,column.getFieldNameLine()));
        return typedThis;
    }
}
