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
import java.util.Arrays;
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
    private List<CompareCondition> compareConditionList = new ArrayList<>();

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
        return getBaseCondition(column,value);
    }

    @Override
    public Children eq(boolean condition, String column, Object value) {
        return condition ? eq(column,value) : typedThis;
    }

    @Override
    public Children eq(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children ne(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? ne(column,value) : typedThis;
    }

    @Override
    public Children ne(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children ne(boolean condition, String column, Object value) {
        return condition ? ne(column,value) : typedThis;
    }

    @Override
    public Children ne(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children lt(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? lt(column,value) : typedThis;
    }

    @Override
    public Children lt(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children lt(boolean condition, String column, Object value) {
        return condition ? lt(column,value) : typedThis;
    }

    @Override
    public Children lt(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children lte(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? lte(column,value) : typedThis;
    }

    @Override
    public Children lte(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children lte(boolean condition, String column, Object value) {
        return condition ? lt(column,value) : typedThis;
    }

    @Override
    public Children lte(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children gt(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? gt(column,value) : typedThis;
    }

    @Override
    public Children gt(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children gt(boolean condition, String column, Object value) {
        return condition ? gt(column,value) : typedThis;
    }

    @Override
    public Children gt(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children gte(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? gte(column,value) : typedThis;
    }

    @Override
    public Children gte(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children gte(boolean condition, String column, Object value) {
        return condition ? gte(column,value) : typedThis;
    }

    @Override
    public Children gte(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children like(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? like(column,value) : typedThis;
    }

    @Override
    public Children like(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children like(boolean condition, String column, Object value) {
        return condition ? like(column,value) : typedThis;
    }

    @Override
    public Children like(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children in(boolean condition, SFunction<T, Object> column, Collection<Object> valueList) {
        return condition ? in(column,valueList) : typedThis;
    }

    @Override
    public Children in(SFunction<T, Object> column, Collection<Object> valueList) {
        return getBaseCondition(column,valueList);
    }

    @Override
    public Children in(boolean condition, String column, Collection<Object> valueList) {
        return condition ? in(column,valueList) : typedThis;
    }

    @Override
    public Children in(String column, Collection<Object> valueList) {
        return getBaseCondition(column,valueList);
    }

    @Override
    public Children nin(boolean condition, SFunction<T, Object> column, Collection<Object> valueList) {
        return condition ? nin(column,valueList) : typedThis;
    }

    @Override
    public Children nin(SFunction<T, Object> column, Collection<Object> valueList) {
        return getBaseCondition(column,valueList);
    }

    @Override
    public Children nin(boolean condition, String column, Collection<Object> valueList) {
        return condition ? nin(column,valueList) : typedThis;
    }

    @Override
    public Children nin(String column, Collection<Object> valueList) {
        return getBaseCondition(column,valueList);
    }

    @Override
    public Children and(boolean condition, LambdaQueryChainWrapper<T> lambdaQueryChainWrapper) {
        return condition ? and(lambdaQueryChainWrapper) : typedThis;
    }

    @Override
    public Children and(LambdaQueryChainWrapper<T> lambdaQueryChainWrapper) {
        return getBaseAndCondition(lambdaQueryChainWrapper.getCompareList());
    }

    @Override
    public Children or(boolean condition, LambdaQueryChainWrapper<T> lambdaQueryChainWrapper) {
        return condition ? or(lambdaQueryChainWrapper) : typedThis;
    }

    @Override
    public Children or(LambdaQueryChainWrapper<T> lambdaQueryChainWrapper) {
        return getBaseOrCondition(lambdaQueryChainWrapper.getCompareList());
    }

    @Override
    public Children or(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? or(column,value) : typedThis;
    }

    @Override
    public Children or(SFunction<T, Object> column, Object value) {
        return getChildBaseCondition(column,value, LogicTypeEnum.OR.getKey());
    }

    @Override
    public Children or(boolean condition, String column, Object value) {
        return condition ? or(column,value) : typedThis;
    }

    @Override
    public Children or(String column, Object value) {
        return getChildBaseCondition(column,value,LogicTypeEnum.OR.getKey());
    }

    @Override
    public Children nor(boolean condition, LambdaQueryChainWrapper<T> lambdaQueryChainWrapper) {
        return condition ? nor(lambdaQueryChainWrapper) : typedThis;
    }

    @Override
    public Children nor(LambdaQueryChainWrapper<T> lambdaQueryChainWrapper) {
        return getBaseOrCondition(lambdaQueryChainWrapper.getCompareList());
    }

    @Override
    public Children nor(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? nor(column,value) : typedThis;
    }

    @Override
    public Children nor(SFunction<T, Object> column, Object value) {
        return getChildBaseCondition(column,value, LogicTypeEnum.NOR.getKey());
    }

    @Override
    public Children nor(boolean condition, String column, Object value) {
        return condition ? nor(column,value) : typedThis;
    }

    @Override
    public Children nor(String column, Object value) {
        return getChildBaseCondition(column,value,LogicTypeEnum.NOR.getKey());
    }

    @Override
    public Children type(SFunction<T, Object> column, TypeEnum value) {
        return getBaseCondition(column,value.getTypeCode());
    }

    @Override
    public Children type(String column, TypeEnum value) {
        return getBaseCondition(column,value.getTypeCode());
    }

    @Override
    public Children type(SFunction<T, Object> column, String value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children type(String column, String value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children type(SFunction<T, Object> column, Integer value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children type(String column, Integer value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children exists(boolean condition, SFunction<T, Object> column, Boolean value) {
        return condition ? exists(column,value) : typedThis;
    }

    @Override
    public Children exists(SFunction<T, Object> column, Boolean value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children exists(boolean condition, String column, Boolean value) {
        return condition ? exists(column,value) : typedThis;
    }

    @Override
    public Children exists(String column, Boolean value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children not() {
        return null;
    }

    @Override
    public Children expr() {
        return null;
    }

    @Override
    public Children mod(boolean condition, SFunction<T, Object> column, long divide, long remain) {
        return condition ? mod(column,divide,remain) : typedThis;
    }

    @Override
    public Children mod(SFunction<T, Object> column, long divide, long remain) {
        return mod(column, Arrays.asList(divide,remain));
    }

    @Override
    public Children mod(boolean condition, SFunction<T, Object> column, Collection<Long> value) {
        return condition ? mod(column,value) : typedThis;
    }

    @Override
    public Children mod(SFunction<T, Object> column, Collection<Long> value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children mod(boolean condition, String column, long divide, long remain) {
        return condition ? mod(column,divide,remain) : typedThis;
    }

    @Override
    public Children mod(String column, long divide, long remain) {
        return mod(column,Arrays.asList(divide,remain));
    }

    @Override
    public Children mod(boolean condition, String column, Collection<Long> value) {
        return condition ? mod(column,value) : typedThis;
    }

    @Override
    public Children mod(String column, Collection<Long> value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children elemMatch(boolean condition, SFunction<T, Object> column, LambdaQueryChainWrapper<T> lambdaQueryChainWrapper) {
        return condition ? elemMatch(column,lambdaQueryChainWrapper) : typedThis;
    }

    @Override
    public Children elemMatch(SFunction<T, Object> column, LambdaQueryChainWrapper<T> lambdaQueryChainWrapper) {
        return getChildBaseCondition(column,lambdaQueryChainWrapper.getCompareList(),LogicTypeEnum.ELEMMATCH.getKey());
    }

    @Override
    public Children elemMatch(boolean condition, String column, LambdaQueryChainWrapper<T> lambdaQueryChainWrapper) {
        return condition ? elemMatch(column,lambdaQueryChainWrapper) : typedThis;
    }

    @Override
    public Children elemMatch(String column, LambdaQueryChainWrapper<T> lambdaQueryChainWrapper) {
        return getChildBaseCondition(column,lambdaQueryChainWrapper.getCompareList(),LogicTypeEnum.ELEMMATCH.getKey());
    }

    @Override
    public Children all(boolean condition, SFunction<T, Object> column, Collection<Object> value) {
        return condition ? all(column,value) : typedThis;
    }

    @Override
    public Children all(SFunction<T, Object> column, Collection<Object> value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children all(boolean condition, String column, Collection<Object> value) {
        return condition ? all(column,value) : typedThis;
    }

    @Override
    public Children all(String column, Collection<Object> value) {
        return getBaseCondition(column,value);
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

    public Children getBaseCondition(String column, Object value){
        compareConditionList.add(new CompareCondition(new Throwable().getStackTrace()[1].getMethodName(), column,value, CpmpareEnum.QUERY.getKey(), LogicTypeEnum.AND.getKey()));
        return typedThis;
    }

    public Children getChildBaseCondition(String column, Object value,Integer logic){
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

    public Children getBaseCondition(SFunction<T, Object> column, Object value){
        compareConditionList.add(new CompareCondition(new Throwable().getStackTrace()[1].getMethodName(), column.getFieldNameLine(),value,CpmpareEnum.QUERY.getKey(),LogicTypeEnum.AND.getKey()));
        return typedThis;
    }

    public Children getChildBaseCondition(SFunction<T,Object> column,Object value,Integer logic){
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
