package com.anwen.mongo.conditions;

import com.anwen.mongo.conditions.interfaces.Compare;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conditions.query.QueryWrapper;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.enums.ProjectionEnum;
import com.anwen.mongo.enums.TypeEnum;
import com.anwen.mongo.support.SFunction;
import com.mongodb.BasicDBObject;
import org.bson.conversions.Bson;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.regex.Pattern;

/**
 * 查询条件
 * @author JiaChaoYang
 * @date 2023/6/24/024 0:49
 */
public abstract class AbstractChainWrapper<T, Children extends AbstractChainWrapper<T, Children>> implements Compare<T,Children> {

    protected final Children typedThis = (Children) this;

    /**
     * 构建条件对象
     * @since 2023/2/10 12:00
     */
    private final List<CompareCondition> compareList = new ArrayList<>();

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
     * 自定义条件语句
     * @author JiaChaoYang
     * @date 2023/8/20 19:40
     */
    List<BasicDBObject> basicDBObjectList = new ArrayList<>();

    public Children getTypedThis() {
        return typedThis;
    }

    public List<CompareCondition> getCompareList() {
        return compareList;
    }

    public List<Order> getOrderList() {
        return orderList;
    }

    public List<Projection> getProjectionList() {
        return projectionList;
    }

    public List<BasicDBObject> getBasicDBObjectList() {
        return basicDBObjectList;
    }

    @Override
    public Children eq(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? eq(column,value) : typedThis;
    }

    @Override
    public Children eq(SFunction<T, Object> column, Object value) {
        return eq(column.getFieldNameLine(),value);
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
        return ne(column.getFieldNameLine(),value);
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
        return lt(column.getFieldNameLine(),value);
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
        return lte(column.getFieldNameLine(),value);
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
        return gt(column.getFieldNameLine(),value);
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
        return gte(column.getFieldNameLine(),value);
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
        return like(column.getFieldNameLine(),value);
    }

    @Override
    public Children like(boolean condition, String column, Object value) {
        return condition ? like(column,value) : typedThis;
    }

    @Override
    public Children like(String column, Object value) {
        if (value instanceof Pattern){
            value = ((Pattern) value).pattern();
        }
        return getBaseCondition(column,value.toString());
    }

    @Override
    public Children likeLeft(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? likeLeft(column,value) : typedThis;
    }

    @Override
    public Children likeLeft(SFunction<T, Object> column, Object value) {
        return likeLeft(column.getFieldNameLine(),value);
    }

    @Override
    public Children likeLeft(boolean condition, String column, Object value) {
        return condition ? likeLeft(column,value) : typedThis;
    }

    @Override
    public Children likeLeft(String column, Object value) {
        return like(column,"^"+value);
    }

    @Override
    public Children likeRight(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? likeRight(column,value) : typedThis;
    }

    @Override
    public Children likeRight(SFunction<T, Object> column, Object value) {
        return likeRight(column.getFieldNameLine(),value);
    }

    @Override
    public Children likeRight(boolean condition, String column, Object value) {
        return condition ? likeRight(column,value) : typedThis;
    }

    @Override
    public Children likeRight(String column, Object value) {
        return like(column,value+"$");
    }

    @Override
    public Children in(boolean condition, SFunction<T, Object> column, Collection<?> valueList) {
        return condition ? in(column,valueList) : typedThis;
    }

    @Override
    public Children in(SFunction<T, Object> column, Collection<?> valueList) {
        return in(column.getFieldNameLine(),valueList);
    }

    @Override
    public Children in(boolean condition, String column, Collection<?> valueList) {
        return condition ? in(column,valueList) : typedThis;
    }

    @Override
    public Children in(String column, Collection<?> valueList) {
        return getBaseCondition(column,valueList);
    }

    @Override
    public Children nin(boolean condition, SFunction<T, Object> column, Collection<?> valueList) {
        return condition ? nin(column,valueList) : typedThis;
    }

    @Override
    public Children nin(SFunction<T, Object> column, Collection<?> valueList) {
        return nin(column.getFieldNameLine(),valueList);
    }

    @Override
    public Children nin(boolean condition, String column, Collection<?> valueList) {
        return condition ? nin(column,valueList) : typedThis;
    }

    @Override
    public Children nin(String column, Collection<?> valueList) {
        return getBaseCondition(column,valueList);
    }

    @Override
    public Children and(boolean condition, QueryChainWrapper<?,?> queryChainWrapper) {
        return condition ? and(queryChainWrapper) : typedThis;
    }

    @Override
    public Children and(QueryChainWrapper<?,?> queryChainWrapper) {
        return getBaseCondition(queryChainWrapper.getCompareList());
    }

    @Override
    public Children and(boolean condition, SFunction<QueryChainWrapper<T,?>,QueryChainWrapper<T,?>> function){
        return condition ? and(function) : typedThis;
    }

    @Override
    public Children and(SFunction<QueryChainWrapper<T,?>,QueryChainWrapper<T,?>> function){
        return and(function.apply(new QueryWrapper<>()));
    }

    @Override
    public Children or(boolean condition, QueryChainWrapper<?,?> queryChainWrapper) {
        return condition ? or(queryChainWrapper) : typedThis;
    }

    @Override
    public Children or(QueryChainWrapper<?,?> queryChainWrapper) {
        return getBaseCondition(queryChainWrapper.getCompareList());
    }

    @Override
    public Children or(boolean condition, SFunction<QueryChainWrapper<T, ?>, QueryChainWrapper<T, ?>> function) {
        return condition ? or(function) : typedThis;
    }

    @Override
    public Children or(SFunction<QueryChainWrapper<T, ?>, QueryChainWrapper<T, ?>> function) {
        return or(function.apply(new QueryWrapper<>()));
    }

    @Override
    public Children nor(boolean condition, QueryChainWrapper<?,?> queryChainWrapper) {
        return condition ? nor(queryChainWrapper) : typedThis;
    }

    @Override
    public Children nor(QueryChainWrapper<?,?> queryChainWrapper) {
        return getBaseCondition(queryChainWrapper.getCompareList());
    }

    @Override
    public Children nor(boolean condition, SFunction<QueryChainWrapper<T, ?>, QueryChainWrapper<T, ?>> function) {
        return condition ? nor(function) : typedThis;
    }

    @Override
    public Children nor(SFunction<QueryChainWrapper<T, ?>, QueryChainWrapper<T, ?>> function) {
        return nor(function.apply(new QueryWrapper<>()));
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
        compareList.add(compareCondition);
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
        compareList.add(compareCondition);
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
    public Children elemMatch(boolean condition, SFunction<T,Object> column, QueryChainWrapper<?, ?> queryChainWrapper) {
        return condition ? elemMatch(column,queryChainWrapper) : typedThis;
    }

    @Override
    public Children elemMatch(SFunction<T,Object> column, QueryChainWrapper<?,?> queryChainWrapper) {
        return elemMatch(column.getFieldNameLine(),queryChainWrapper);
    }

    @Override
    public Children elemMatch(boolean condition, String column, QueryChainWrapper<?,?> queryChainWrapper) {
        return condition ? elemMatch(column,queryChainWrapper) : typedThis;
    }

    @Override
    public Children elemMatch(String column, QueryChainWrapper<?,?> queryChainWrapper) {
        return getBaseCondition(column,queryChainWrapper.getCompareList());
    }

    @Override
    public Children all(boolean condition, SFunction<T, Object> column, Collection<?> value) {
        return condition ? all(column,value) : typedThis;
    }

    @Override
    public Children all(SFunction<T, Object> column, Collection<?> value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children all(boolean condition, String column, Collection<?> value) {
        return condition ? all(column,value) : typedThis;
    }

    @Override
    public Children all(String column, Collection<?> value) {
        return getBaseCondition(column,value);
    }

    @Override
    public Children regex(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? regex(column,value) : typedThis;
    }

    @Override
    public Children regex(SFunction<T, Object> column, Object value) {
        return regex(column.getFieldNameLine(),value);
    }

    @Override
    public Children regex(boolean condition, String column, Object value) {
        return condition ? regex(column,value) : typedThis;
    }

    @Override
    public Children regex(String column, Object value) {
        if (value instanceof Pattern){
            value = ((Pattern) value).pattern();
        }
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

    @Override
    public Children between(boolean condition, SFunction<T, Object> column, Object gte, Object lte, boolean convertGtOrLt) {
        return condition ? between(column,gte,lte,convertGtOrLt) : typedThis;
    }

    @Override
    public Children between(SFunction<T, Object> column, Object gte, Object lte, boolean convertGtOrLt) {
        return getBaseConditionBetween(column.getFieldNameLine(),gte,lte,convertGtOrLt);
    }

    @Override
    public Children between(boolean condition, String column, Object gte, Object lte, boolean convertGtOrLt) {
        return condition ? between(column,gte,lte,convertGtOrLt) : typedThis;
    }

    @Override
    public Children between(String column, Object gte, Object lte, boolean convertGtOrLt) {
        return getBaseConditionBetween(column,gte,lte,convertGtOrLt);
    }

    @Override
    public Children where(String javaScriptExpression) {
        return getBaseCondition(javaScriptExpression);
    }

    @Override
    public Children size(SFunction<T, ?> fieldName, int size) {
        return size(fieldName.getFieldNameLine(),size);
    }

    @Override
    public Children size(String fieldName, int size) {
        return getBaseCondition(fieldName,size);
    }

    @Override
    public Children bitsAllClear(SFunction<T, ?> fieldName, long bitmask) {
        return bitsAllClear(fieldName.getFieldNameLine(),bitmask);
    }

    @Override
    public Children bitsAllClear(String fieldName, long bitmask) {
        return getBaseCondition(fieldName,bitmask);
    }

    @Override
    public Children bitsAllSet(SFunction<T, ?> fieldName, long bitmask) {
        return bitsAllSet(fieldName.getFieldNameLine(),bitmask);
    }

    @Override
    public Children bitsAllSet(String fieldName, long bitmask) {
        return getBaseCondition(fieldName,bitmask);
    }

    @Override
    public Children bitsAnyClear(SFunction<T, ?> fieldName, long bitmask) {
        return bitsAnyClear(fieldName.getFieldNameLine(),bitmask);
    }

    @Override
    public Children bitsAnyClear(String fieldName, long bitmask) {
        return getBaseCondition(fieldName,bitmask);
    }

    @Override
    public Children bitsAnySet(SFunction<T, ?> fieldName, long bitmask) {
        return bitsAnySet(fieldName.getFieldNameLine(),bitmask);
    }

    @Override
    public Children bitsAnySet(String fieldName, long bitmask) {
        return getBaseCondition(fieldName,bitmask);
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
    public Children custom(List<BasicDBObject> basicDBObjectList) {
        this.basicDBObjectList.addAll(basicDBObjectList);
        return typedThis;
    }

    public Children getBaseConditionBetween(String column,Object gte,Object lte,boolean convertGtOrLt){
        compareList.add(CompareCondition.builder().condition(!convertGtOrLt ? "gte" : "gt").column(column).value(gte).build());
        compareList.add(CompareCondition.builder().condition(!convertGtOrLt ? "lte" : "lt").column(column).value(lte).build());
        return typedThis;
    }

    public Children getBaseCondition(String column, Object value){
        this.compareList.add(new CompareCondition(Thread.currentThread().getStackTrace()[2].getMethodName(),column,value));
        return typedThis;
    }

    public Children getBaseCondition(SFunction<T, Object> column, Object value){
        return getBaseCondition(column.getFieldNameLine(),value);
    }

    public Children getBaseCondition(List<CompareCondition> compareConditionList){
        this.compareList.add(new CompareCondition(Thread.currentThread().getStackTrace()[2].getMethodName(),compareConditionList));
        return typedThis;
    }

    public Children getBaseCondition(String value){
        this.compareList.add(new CompareCondition(Thread.currentThread().getStackTrace()[2].getMethodName(),value));
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

    public Children getBaseProject(Projection... projections){
        projectionList.addAll(Arrays.asList(projections));
        return typedThis;
    }

    public Children getBaseProjectDisplay(String... columns){
        for (String column : columns) {
            projectionList.add(Projection.builder().column(column).value(ProjectionEnum.DISPLAY.getValue()).build());
        }
        return typedThis;
    }

    public Children getBaseProjectNone(String... columns){
        for (String column : columns) {
            projectionList.add(Projection.builder().column(column).value(ProjectionEnum.NONE.getValue()).build());
        }
        return typedThis;
    }

    @SafeVarargs
    public final Children getBaseProjectDisplay(SFunction<T, Object>... columns){
        for (SFunction<T, Object> column : columns) {
            projectionList.add(Projection.builder().column(column.getFieldNameLine()).value(ProjectionEnum.DISPLAY.getValue()).build());
        }
        return typedThis;
    }

    @SafeVarargs
    public final Children getBaseProjectNone(SFunction<T, Object>... columns){
        for (SFunction<T, Object> column : columns) {
            projectionList.add(Projection.builder().column(column.getFieldNameLine()).value(ProjectionEnum.NONE.getValue()).build());
        }
        return typedThis;
    }

    public Children setProjectNoneId(){
        projectionList.add(Projection.builder().column(SqlOperationConstant._ID).value(ProjectionEnum.NONE.getValue()).build());
        return typedThis;
    }

}
