package com.anwen.mongo.sql.conditions;

import cn.hutool.json.JSONUtil;
import com.anwen.mongo.enums.CompareEnum;
import com.anwen.mongo.enums.LogicTypeEnum;
import com.anwen.mongo.enums.TypeEnum;
import com.anwen.mongo.sql.conditions.interfaces.Compare;
import com.anwen.mongo.sql.interfaces.CompareCondition;
import com.anwen.mongo.sql.interfaces.Order;
import com.anwen.mongo.sql.interfaces.Projection;
import com.anwen.mongo.sql.query.LambdaQueryChainWrapper;
import com.anwen.mongo.sql.support.SFunction;
import lombok.Getter;

import java.lang.reflect.Method;
import java.util.*;

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
     * 构建显示字段
     * @author JiaChaoYang
     * @date 2023/7/30 20:34
    */
    List<Projection> projectionList = new ArrayList<>();

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
    public Children not(CompareCondition compareCondition) {
        compareConditionList.add(compareCondition);
        return typedThis;
    }

    @Override
    public Children not(boolean condition, CompareCondition compareCondition) {
        return condition ? not(compareCondition) : typedThis;
    }

    @Override
    public Children expr(boolean condition, CompareCondition compareCondition) {
        return condition ? expr(compareCondition) : typedThis;
    }
    @Override
    public Children expr(CompareCondition compareCondition) {
        compareConditionList.add(compareCondition);
        return typedThis;
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
    public Children regex(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? regex(column,value) : typedThis;
    }

    @Override
    public Children regex(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children regex(boolean condition, String column, Object value) {
        return condition ? regex(column,value) : typedThis;
    }

    @Override
    public Children regex(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children text(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? text(column,value) : typedThis;
    }

    @Override
    public Children text(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children text(boolean condition, String column, Object value) {
        return condition ? text(column,value) : typedThis;
    }

    @Override
    public Children text(String column, Object value) {
        return getBaseCondition(column,value);
    }

    public Children getBaseCondition(String column, Object value){
        compareConditionList.add(CompareCondition.builder().condition(new Throwable().getStackTrace()[1].getMethodName()).column(column).value(value).type(CompareEnum.QUERY.getKey()).logicType(LogicTypeEnum.AND.getKey()).build());
        return typedThis;
    }

    public Children getChildBaseCondition(String column, Object value,Integer logic){
        compareConditionList.add(CompareCondition.builder().condition(new Throwable().getStackTrace()[1].getMethodName()).column(column).value(value).type(CompareEnum.QUERY.getKey()).logicType(logic).build());
        return typedThis;
    }

    public Children getBaseOrCondition(List<CompareCondition> compareConditionList){
        this.compareConditionList.add(CompareCondition.builder().type(CompareEnum.QUERY.getKey()).logicType(LogicTypeEnum.OR.getKey()).childCondition(compareConditionList).build());
        return typedThis;
    }

    public Children getBaseAndCondition(List<CompareCondition> compareConditionList){
        CompareCondition compareCondition = new CompareCondition();
        compareCondition.setCondition("and");
        compareCondition.setType(CompareEnum.QUERY.getKey());
        compareCondition.setLogicType(LogicTypeEnum.AND.getKey());
        compareCondition.setChildCondition(compareConditionList);
        this.compareConditionList.add(compareCondition);
        return typedThis;
    }

    public Children getBaseCondition(SFunction<T, Object> column, Object value){
        compareConditionList.add(CompareCondition.builder().condition(new Throwable().getStackTrace()[1].getMethodName()).column(column.getFieldNameLine()).value(value).type(CompareEnum.QUERY.getKey()).logicType(LogicTypeEnum.AND.getKey()).build());
        return typedThis;
    }

    public Children getChildBaseCondition(SFunction<T,Object> column,Object value,Integer logic){
        compareConditionList.add(CompareCondition.builder().condition(new Throwable().getStackTrace()[1].getMethodName()).column(column.getFieldNameLine()).value(value).type(CompareEnum.QUERY.getKey()).logicType(logic).build());
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

    public static Set<String> getMethodNames(Class<?> clazz) {
        Set<String> methodNames = new HashSet<>();

        // 获取类中声明的所有方法
        Method[] methods = clazz.getDeclaredMethods();

        // 遍历每个方法并获取方法名
        for (Method method : methods) {
            methodNames.add(method.getName());
        }

        return methodNames;
    }

    public static void main(String[] args) {
        // 示例使用：获取String类中的所有方法名
        Set<String> stringMethodNames = getMethodNames(AbstractChainWrapper.class);
        System.out.println(JSONUtil.toJsonStr(stringMethodNames));
    }

}
