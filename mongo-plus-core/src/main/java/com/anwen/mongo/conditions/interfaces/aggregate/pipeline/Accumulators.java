package com.anwen.mongo.conditions.interfaces.aggregate.pipeline;

import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.project.SimpleExpression;
import com.anwen.mongo.support.SFunction;
import com.mongodb.BasicDBObject;
import com.mongodb.client.model.BsonField;
import com.mongodb.lang.Nullable;
import org.bson.BsonArray;
import org.bson.BsonDocument;
import org.bson.BsonString;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.util.Arrays;
import java.util.List;

import static com.mongodb.assertions.Assertions.notNull;
import static java.util.stream.Collectors.toList;

/**
 * 构建累加器
 *
 * @author anwen
 * @date 2024/6/12 下午3:44
 */
public final class Accumulators {

    /**
     * 获取 $group 操作的字段名称，该字段表示将给定表达式应用于组内所有成员时的值的总和。
     *
     * @param fieldName 字段名称
     * @param expression 表达式
     * @param <TExpression> 表达式类型
     * @return $sum
     * @since mongodb.driver.manual reference/operator/aggregation/sum/ $sum
     */
    public static <TExpression> BsonField sum(final String fieldName, final TExpression expression) {
        return accumulatorOperator("$sum", fieldName, expression);
    }

    /**
     * 获取 $group 操作的字段名称，该字段表示将给定表达式应用于组内所有成员时的值的总和。
     *
     * @param fieldName 字段名称
     * @param expression 表达式
     * @param <TExpression> 表达式类型
     * @return $sum
     * @since mongodb.driver.manual reference/operator/aggregation/sum/ $sum
     */
    public static <TExpression,T> BsonField sum(final SFunction<T,?> fieldName, final TExpression expression) {
        return sum(fieldName.getFieldNameLine(),expression);
    }

    /**
     * 获取 $group 操作的字段名称，该字段表示将给定表达式应用于组内所有成员时的平均值。
     *
     * @param fieldName 字段名称
     * @param expression 表达式
     * @param <TExpression> 表达式类型
     * @return $avg
     * @since mongodb.driver.manual reference/operator/aggregation/avg/ $avg
     */
    public static <TExpression> BsonField avg(final String fieldName, final TExpression expression) {
        return accumulatorOperator("$avg", fieldName, expression);
    }

    /**
     * 获取 $group 操作的字段名称，该字段表示将给定表达式应用于组内所有成员时的平均值。
     *
     * @param fieldName 字段名称
     * @param expression 表达式
     * @param <TExpression> 表达式类型
     * @return $avg
     * @since mongodb.driver.manual reference/operator/aggregation/avg/ $avg
     */
    public static <TExpression,T> BsonField avg(final SFunction<T,?> fieldName, final TExpression expression) {
        return avg(fieldName.getFieldNameLine(),expression);
    }

    /**
     * 获取 $group 操作的字段名称，该字段名称表示应用于组的第一个成员时给定表达式的值。
     *
     * @param fieldName 字段名称
     * @param expression 表达式
     * @param <TExpression> 表达式类型
     * @return $first
     * @since mongodb.driver.manual reference/operator/aggregation/first/ $first
     */
    public static <TExpression> BsonField first(final String fieldName, final TExpression expression) {
        return accumulatorOperator("$first", fieldName, expression);
    }

    /**
     * 获取 $group 操作的字段名称，该字段名称表示应用于组的第一个成员时给定表达式的值。
     *
     * @param fieldName 字段名称
     * @param expression 表达式
     * @param <TExpression> 表达式类型
     * @return $first
     * @since mongodb.driver.manual reference/operator/aggregation/first/ $first
     */
    public static <TExpression,T> BsonField first(final SFunction<T,?> fieldName, final TExpression expression) {
        return first(fieldName.getFieldNameLine(),expression);
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含给定{@code inExpression}的值，该值是根据预排序组中的前{@code N}个元素计算的，其中N是{@code nExpression}的正整数值。
     *
     * @param fieldName 由累加器计算的字段。
     * @param inExpression 输入表达式
     * @param nExpression 限制产生的值的数量的表达式
     * @param <InExpression> 输入表达式的类型.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/firstN/ $firstN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <InExpression, NExpression> BsonField firstN(
            final String fieldName, final NExpression nExpression, final InExpression inExpression) {
        return pickNAccumulator(notNull("fieldName", fieldName), "$firstN",
                notNull("inExpression", inExpression), notNull("nExpression", nExpression));
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含给定{@code inExpression}的值，该值是根据预排序组中的前{@code N}个元素计算的，其中N是{@code nExpression}的正整数值。
     *
     * @param fieldName 由累加器计算的字段。
     * @param inExpression 输入表达式
     * @param nExpression 限制产生的值的数量的表达式
     * @param <InExpression> 输入表达式的类型.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/firstN/ $firstN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <InExpression, NExpression,T> BsonField firstN(
            final SFunction<T,?> fieldName, final NExpression nExpression, final InExpression inExpression) {
        return firstN(fieldName.getFieldNameLine(),nExpression,inExpression);
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含给定{@code inExpression}的值，该值是根据预排序组中的前{@code N}个元素计算的，其中N是{@code nExpression}的正整数值。
     *
     * @param fieldName 由累加器计算的字段。
     * @param inExpression 输入表达式
     * @param nExpression 限制产生的值的数量的表达式
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/firstN/ $firstN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression,T,R> BsonField firstN(
            final SFunction<T,?> fieldName, final NExpression nExpression, final SFunction<R,?> inExpression) {
        return firstN(fieldName.getFieldNameLine(),nExpression,inExpression.getFieldNameLineOption());
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含给定{@code inExpression}的值，该值是根据预排序组中的前{@code N}个元素计算的，其中N是{@code nExpression}的正整数值。
     *
     * @param fieldName 由累加器计算的字段。
     * @param inExpression 输入表达式
     * @param nExpression 限制产生的值的数量的表达式
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/firstN/ $firstN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression,T> BsonField firstN(
            final String fieldName, final NExpression nExpression, final SFunction<T,?> inExpression) {
        return firstN(fieldName,nExpression,inExpression.getFieldNameLineOption());
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含给定{@code inExpression}的值，该值是根据预排序组中的前{@code N}个元素计算的，其中N是{@code nExpression}的正整数值。
     *
     * @param fieldName 由累加器计算的字段。
     * @param inExpression 输入表达式
     * @param nExpression 限制产生的值的数量的表达式
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/firstN/ $firstN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression,T> BsonField firstN(
            final String fieldName, final NExpression nExpression, final SFunction<T,?>... inExpression) {
        return firstN(fieldName, Arrays.stream(inExpression).map(SFunction::getFieldNameLineOption).collect(toList()), nExpression);
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含给定{@code inExpression}的值，该值是根据预排序组中的前{@code N}个元素计算的，其中N是{@code nExpression}的正整数值。
     *
     * @param fieldName 由累加器计算的字段。
     * @param inExpression 输入表达式
     * @param nExpression 限制产生的值的数量的表达式
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/firstN/ $firstN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression> BsonField firstN(
            final String fieldName, final NExpression nExpression, final String... inExpression) {
        return firstN(fieldName, nExpression, Arrays.stream(inExpression).collect(toList()));
    }

    /**
     * 返回计算字段和累加器的组合，该组合产生根据提供的sortBy规范排序的组内顶部元素计算的给定outExpression的值。
     *
     * @param fieldName 由累加器计算的字段
     * @param sortBy {@linkplain Sorts 排序规范}。语法与 {@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}所期望的语法相同。
     * @param outExpression 输出表达式.
     * @param <OutExpression> 输出表达式的类型.
     * @return The requested {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/top/ $top
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <OutExpression> BsonField top(final String fieldName, final Bson sortBy, final OutExpression outExpression) {
        return sortingPickAccumulator(notNull("fieldName", fieldName), "$top",
                notNull("sortBy", sortBy), notNull("outExpression", outExpression));
    }

    /**
     * 返回计算字段和累加器的组合，该组合产生根据提供的sortBy规范排序的组内顶部元素计算的给定outExpression的值。
     *
     * @param fieldName 由累加器计算的字段
     * @param sortBy {@linkplain Sorts 排序规范}。语法与 {@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}所期望的语法相同。
     * @param outExpression 输出表达式.
     * @param <OutExpression> 输出表达式的类型.
     * @return The requested {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/top/ $top
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <OutExpression,T> BsonField top(final SFunction<T,?> fieldName, final Bson sortBy, final OutExpression outExpression) {
        return top(fieldName.getFieldNameLine(),sortBy,outExpression);
    }

    /**
     * 返回计算字段和累加器的组合，该组合产生根据提供的sortBy规范排序的组内顶部元素计算的给定outExpression的值。
     *
     * @param fieldName 由累加器计算的字段
     * @param sortBy {@linkplain Sorts 排序规范}。语法与 {@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}所期望的语法相同。
     * @param outExpression 输出表达式.
     * @return The requested {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/top/ $top
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <T,R> BsonField top(final SFunction<T,?> fieldName, final Bson sortBy, final SFunction<R,?>... outExpression) {
        return top(fieldName.getFieldNameLine(),sortBy, Arrays.stream(outExpression).map(SFunction::getFieldNameLineOption).collect(toList()));
    }

    /**
     * 返回计算字段和累加器的组合，该组合产生根据提供的sortBy规范排序的组内顶部元素计算的给定outExpression的值。
     *
     * @param fieldName 由累加器计算的字段
     * @param sortBy {@linkplain Sorts 排序规范}。语法与 {@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}所期望的语法相同。
     * @param outExpression 输出表达式.
     * @return The requested {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/top/ $top
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <T> BsonField top(final SFunction<T,?> fieldName, final Bson sortBy, final String... outExpression) {
        return top(fieldName.getFieldNameLine(),sortBy, Arrays.stream(outExpression).collect(toList()));
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含根据提供的{@code sortBy}规范排序的组内前{@code N}个元素计算的给定{@code outExpression}的值，其中{@code N}是{@code nExpression}的正整数值。
     *
     * @param fieldName 由累加器计算的字段.
     * @param sortBy {@linkplain Sorts 排序规范}。语法与 {@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}所期望的语法相同。
     * @param outExpression 输出表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <OutExpression> 输出表达式的类型.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/topN/ $topN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <OutExpression, NExpression> BsonField topN(
            final String fieldName, final Bson sortBy, final NExpression nExpression, final OutExpression outExpression) {
        return sortingPickNAccumulator(notNull("fieldName", fieldName), "$topN",
                notNull("sortBy", sortBy), notNull("outExpression", outExpression), notNull("nExpression", nExpression));
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含根据提供的{@code sortBy}规范排序的组内前{@code N}个元素计算的给定{@code outExpression}的值，其中{@code N}是{@code nExpression}的正整数值。
     *
     * @param fieldName 由累加器计算的字段.
     * @param sortBy {@linkplain Sorts 排序规范}。语法与 {@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}所期望的语法相同。
     * @param outExpression 输出表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <OutExpression> 输出表达式的类型.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/topN/ $topN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <OutExpression, NExpression,T> BsonField topN(
            final SFunction<T,?> fieldName, final Bson sortBy, final NExpression nExpression, final OutExpression outExpression) {
        return topN(fieldName.getFieldNameLine(), sortBy, nExpression,outExpression);
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含根据提供的{@code sortBy}规范排序的组内前{@code N}个元素计算的给定{@code outExpression}的值，其中{@code N}是{@code nExpression}的正整数值。
     *
     * @param fieldName 由累加器计算的字段.
     * @param sortBy {@linkplain Sorts 排序规范}。语法与 {@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}所期望的语法相同。
     * @param outExpression 输出表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/topN/ $topN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression,T,R> BsonField topN(
            final SFunction<T,?> fieldName, final Bson sortBy, final NExpression nExpression, final SFunction<R,?> outExpression) {
        return topN(fieldName.getFieldNameLine(), sortBy,nExpression,outExpression);
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含根据提供的{@code sortBy}规范排序的组内前{@code N}个元素计算的给定{@code outExpression}的值，其中{@code N}是{@code nExpression}的正整数值。
     *
     * @param fieldName 由累加器计算的字段.
     * @param sortBy {@linkplain Sorts 排序规范}。语法与 {@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}所期望的语法相同。
     * @param outExpression 输出表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/topN/ $topN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression,T,R> BsonField topN(
            final SFunction<T,?> fieldName, final Bson sortBy, final NExpression nExpression, final SFunction<R,?>... outExpression) {
        return topN(fieldName.getFieldNameLine(), sortBy,nExpression, Arrays.stream(outExpression).map(SFunction::getFieldNameLineOption).collect(toList()));
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含根据提供的{@code sortBy}规范排序的组内前{@code N}个元素计算的给定{@code outExpression}的值，其中{@code N}是{@code nExpression}的正整数值。
     *
     * @param fieldName 由累加器计算的字段.
     * @param sortBy {@linkplain Sorts 排序规范}。语法与 {@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}所期望的语法相同。
     * @param outExpression 输出表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/topN/ $topN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression,T> BsonField topN(
            final SFunction<T,?> fieldName, final Bson sortBy, final NExpression nExpression, final String... outExpression) {
        return topN(fieldName.getFieldNameLine(), sortBy,nExpression, Arrays.stream(outExpression).collect(toList()));
    }

    /**
     * 获取 $group 操作的字段名称，该字段名称表示应用于组的最后一个成员时给定表达式的值
     *
     * @param fieldName 字段名称
     * @param expression 表达式
     * @return $last
     * @since mongodb.driver.manual reference/operator/aggregation/last/ $last
     */
    public static <TExpression> BsonField last(final String fieldName, final TExpression expression) {
        return accumulatorOperator("$last", fieldName, expression);
    }

    /**
     * 获取 $group 操作的字段名称，该字段名称表示应用于组的最后一个成员时给定表达式的值
     *
     * @param fieldName 字段名称
     * @param expression 表达式
     * @return $last
     * @since mongodb.driver.manual reference/operator/aggregation/last/ $last
     */
    public static <T,R> BsonField last(final SFunction<T,?> fieldName, final SFunction<R,?> expression) {
        return last(fieldName.getFieldNameLine(), expression.getFieldNameLineOption());
    }

    /**
     * 获取 $group 操作的字段名称，该字段名称表示应用于组的最后一个成员时给定表达式的值
     *
     * @param fieldName 字段名称
     * @param expression 表达式
     * @param <TExpression> 表达式类型
     * @return $last
     * @since mongodb.driver.manual reference/operator/aggregation/last/ $last
     */
    public static <TExpression,T> BsonField last(final SFunction<T,?> fieldName, final TExpression expression) {
        return last(fieldName.getFieldNameLine(), expression);
    }

    /**
     * 获取 $group 操作的字段名称，该字段名称表示应用于组的最后一个成员时给定表达式的值
     *
     * @param fieldName 字段名称
     * @param expression 表达式
     * @return $last
     * @since mongodb.driver.manual reference/operator/aggregation/last/ $last
     */
    public static <T> BsonField last(final String fieldName, final SFunction<T,?> expression) {
        return last(fieldName, expression.getFieldNameLineOption());
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含给定{@code inExpression}的值，该值是根据预排序组中的最后{@code N}元素计算的，其中{@code N}是{@code nExpression}的正整数值。
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param inExpression 输入表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <InExpression> 输入表达式的类型.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/lastN/ $lastN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <InExpression, NExpression> BsonField lastN(
            final String fieldName, final NExpression nExpression, final InExpression inExpression) {
        return pickNAccumulator(notNull("fieldName", fieldName), "$lastN",
                notNull("inExpression", inExpression), notNull("nExpression", nExpression));
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含给定{@code inExpression}的值，该值是根据预排序组中的最后{@code N}元素计算的，其中{@code N}是{@code nExpression}的正整数值。
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param inExpression 输入表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <InExpression> 输入表达式的类型.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/lastN/ $lastN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <InExpression, NExpression,T> BsonField lastN(
            final SFunction<T,?> fieldName, final NExpression nExpression, final InExpression inExpression) {
        return lastN(fieldName.getFieldNameLine(),nExpression,inExpression);
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含给定{@code inExpression}的值，该值是根据预排序组中的最后{@code N}元素计算的，其中{@code N}是{@code nExpression}的正整数值。
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param inExpression 输入表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/lastN/ $lastN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression,T,R> BsonField lastN(
            final SFunction<T,?> fieldName, final NExpression nExpression, final SFunction<R,?> inExpression) {
        return lastN(fieldName.getFieldNameLine(),nExpression,inExpression.getFieldNameLineOption());
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含给定{@code inExpression}的值，该值是根据预排序组中的最后{@code N}元素计算的，其中{@code N}是{@code nExpression}的正整数值。
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param inExpression 输入表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/lastN/ $lastN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression,T> BsonField lastN(
            final SFunction<T,?> fieldName, final NExpression nExpression, final String... inExpression) {
        return lastN(fieldName.getFieldNameLine(),nExpression, Arrays.stream(inExpression).collect(toList()));
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含给定{@code inExpression}的值，该值是根据预排序组中的最后{@code N}元素计算的，其中{@code N}是{@code nExpression}的正整数值。
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param inExpression 输入表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/lastN/ $lastN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression,T,R> BsonField lastN(
            final SFunction<T,?> fieldName, final NExpression nExpression, final SFunction<R,?>... inExpression) {
        return lastN(fieldName.getFieldNameLine(),nExpression, Arrays.stream(inExpression).map(SFunction::getFieldNameLineOption).collect(toList()));
    }

    /**
     * Returns a combination of a computed field and an accumulator that produces
     * a value of the given {@code outExpression} computed for the bottom element within a group
     * sorted according to the provided {@code sortBy} specification.
     *
     * @param fieldName {@link BsonField} computed by the accumulator.
     * @param sortBy The {@linkplain Sorts sort specification}. The syntax is identical to the one expected by {@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}.
     * @param outExpression The output expression.
     * @param <OutExpression> The type of the output expression.
     * @return The requested {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/bottom/ $bottom
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <OutExpression> BsonField bottom(final String fieldName, final Bson sortBy, final OutExpression outExpression) {
        return sortingPickAccumulator(notNull("fieldName", fieldName), "$bottom",
                notNull("sortBy", sortBy), notNull("outExpression", outExpression));
    }

    /**
     * 返回计算字段和累加器的组合，该组合产生根据提供的{@code sortBy}规范排序的组内底部元素计算的给定{@code outExpression}的值
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param sortBy {@linkplain Sorts 排序规范}。语法与{@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}所期望的语法相同.
     * @param outExpression 输出表达式.
     * @param <OutExpression> 输出表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/bottom/ $bottom
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <OutExpression,T> BsonField bottom(final SFunction<T,?> fieldName, final Bson sortBy, final OutExpression outExpression) {
        return bottom(fieldName.getFieldNameLine(),sortBy,outExpression);
    }

    /**
     * 返回计算字段和累加器的组合，该组合产生根据提供的{@code sortBy}规范排序的组内底部元素计算的给定{@code outExpression}的值
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param sortBy {@linkplain Sorts 排序规范}。语法与{@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}所期望的语法相同.
     * @param outExpression 输出表达式.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/bottom/ $bottom
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <T,R> BsonField bottom(final SFunction<T,?> fieldName, final Bson sortBy, final SFunction<R,?> outExpression) {
        return bottom(fieldName.getFieldNameLine(),sortBy,outExpression.getFieldNameLineOption());
    }

    /**
     * 返回计算字段和累加器的组合，该组合产生根据提供的{@code sortBy}规范排序的组内底部元素计算的给定{@code outExpression}的值
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param sortBy {@linkplain Sorts 排序规范}。语法与{@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}所期望的语法相同.
     * @param outExpression 输出表达式.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/bottom/ $bottom
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <T,R> BsonField bottom(final SFunction<T,?> fieldName, final Bson sortBy, final SFunction<R,?>... outExpression) {
        return bottom(fieldName.getFieldNameLine(),sortBy, Arrays.stream(outExpression).map(SFunction::getFieldNameLineOption).collect(toList()));
    }

    /**
     * 返回计算字段和累加器的组合，该组合产生根据提供的{@code sortBy}规范排序的组内底部元素计算的给定{@code outExpression}的值
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param sortBy {@linkplain Sorts 排序规范}。语法与{@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}所期望的语法相同.
     * @param outExpression 输出表达式.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/bottom/ $bottom
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <T> BsonField bottom(final SFunction<T,?> fieldName, final Bson sortBy, final String... outExpression) {
        return bottom(fieldName.getFieldNameLine(),sortBy, Arrays.stream(outExpression).collect(toList()));
    }

    /**
     * Returns a combination of a computed field and an accumulator that produces a BSON {@link org.bson.BsonType#ARRAY Array}
     * of values of the given {@code outExpression} computed for the bottom {@code N} elements within a group
     * sorted according to the provided {@code sortBy} specification,
     * where {@code N} is the positive integral value of the {@code nExpression}.
     *
     * @param fieldName The field computed by the accumulator.
     * @param sortBy The {@linkplain Sorts sort specification}. The syntax is identical to the one expected by {@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}.
     * @param outExpression The output expression.
     * @param nExpression The expression limiting the number of produced values.
     * @param <OutExpression> The type of the output expression.
     * @param <NExpression> The type of the limiting expression.
     * @return The requested {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/bottomN/ $bottomN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <OutExpression, NExpression> BsonField bottomN(
            final String fieldName, final Bson sortBy, final NExpression nExpression, final OutExpression outExpression) {
        return sortingPickNAccumulator(notNull("fieldName", fieldName), "$bottomN",
                notNull("sortBy", sortBy), notNull("outExpression", outExpression), notNull("nExpression", nExpression));
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含根据提供的{@code sortBy}规范排序的组内底部{@code N}个元素计算的给定{@code outExpression}的值，其中{@code N}是nExpression的正整数值。
     *
     * @param fieldName 由累加器计算的字段.
     * @param sortBy {@linkplain Sorts 排序规范}。语法与{@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}所期望的语法相同.
     * @param outExpression 输出表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <OutExpression> 输出表达式的类型.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/bottomN/ $bottomN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <OutExpression, NExpression,T> BsonField bottomN(
            final SFunction<T,?> fieldName, final Bson sortBy, final NExpression nExpression, final OutExpression outExpression) {
        return bottomN(fieldName.getFieldNameLine(),sortBy,nExpression,outExpression);
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含根据提供的{@code sortBy}规范排序的组内底部{@code N}个元素计算的给定{@code outExpression}的值，其中{@code N}是nExpression的正整数值。
     *
     * @param fieldName 由累加器计算的字段.
     * @param sortBy {@linkplain Sorts 排序规范}。语法与{@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}所期望的语法相同.
     * @param outExpression 输出表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/bottomN/ $bottomN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression,T,R> BsonField bottomN(
            final SFunction<T,?> fieldName, final Bson sortBy, final NExpression nExpression, final SFunction<R,?> outExpression) {
        return bottomN(fieldName.getFieldNameLine(),sortBy,nExpression,outExpression.getFieldNameLineOption());
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含根据提供的{@code sortBy}规范排序的组内底部{@code N}个元素计算的给定{@code outExpression}的值，其中{@code N}是nExpression的正整数值。
     *
     * @param fieldName 由累加器计算的字段.
     * @param sortBy {@linkplain Sorts 排序规范}。语法与{@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}所期望的语法相同.
     * @param outExpression 输出表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/bottomN/ $bottomN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression,T,R> BsonField bottomN(
            final SFunction<T,?> fieldName, final Bson sortBy, final NExpression nExpression, final SFunction<R,?>... outExpression) {
        return bottomN(fieldName.getFieldNameLine(),sortBy,nExpression, Arrays.stream(outExpression).map(SFunction::getFieldNameLineOption).collect(toList()));
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成一个 BSON {@link org.bson.BsonType#ARRAY Array} ，该数组包含根据提供的{@code sortBy}规范排序的组内底部{@code N}个元素计算的给定{@code outExpression}的值，其中{@code N}是nExpression的正整数值。
     *
     * @param fieldName 由累加器计算的字段.
     * @param sortBy {@linkplain Sorts 排序规范}。语法与{@link com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate#sort(Bson)}所期望的语法相同.
     * @param outExpression 输出表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/bottomN/ $bottomN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression,T> BsonField bottomN(
            final SFunction<T,?> fieldName, final Bson sortBy, final NExpression nExpression, final String... outExpression) {
        return bottomN(fieldName.getFieldNameLine(),sortBy,nExpression, Arrays.stream(outExpression).collect(toList()));
    }

    /**
     * Gets a field name for a $group operation representing the maximum of the values of the given expression when applied to all
     * members of the group.
     *
     * @param fieldName the field name
     * @param expression the expression
     * @param <TExpression> the expression type
     * @return the field
     * @since mongodb.driver.manual reference/operator/aggregation/max/ $max
     */
    public static <TExpression> BsonField max(final String fieldName, final TExpression expression) {
        return accumulatorOperator("$max", fieldName, expression);
    }

    /**
     * 获取 $group 操作的字段名称，该字段名称表示将给定表达式应用于组内所有成员时所得值的最大值.
     *
     * @param fieldName 字段名称
     * @param expression 表达式
     * @param <TExpression> 表达式类型
     * @return $max
     * @since mongodb.driver.manual reference/operator/aggregation/max/ $max
     */
    public static <TExpression,T> BsonField max(final SFunction<T,?> fieldName, final TExpression expression) {
        return max(fieldName.getFieldNameLine(),expression);
    }

    /**
     * 获取 $group 操作的字段名称，该字段名称表示将给定表达式应用于组内所有成员时所得值的最大值.
     *
     * @param fieldName 字段名称
     * @param expression 表达式
     * @return $max
     * @since mongodb.driver.manual reference/operator/aggregation/max/ $max
     */
    public static <T,R> BsonField max(final SFunction<T,?> fieldName, final SFunction<R,?> expression) {
        return max(fieldName.getFieldNameLine(),expression.getFieldNameLineOption());
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成给定{@code inExpression}的{@code N}最大值的 BSON {@link org.bson.BsonType#ARRAY Array} ，其中{@code N}是{@code nExpression}的正整数值
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param inExpression 输入表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <InExpression> 输入表达式的类型.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/maxN/ $maxN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <InExpression, NExpression> BsonField maxN(
            final String fieldName, final NExpression nExpression, final InExpression inExpression) {
        return pickNAccumulator(notNull("fieldName", fieldName), "$maxN",
                notNull("inExpression", inExpression), notNull("nExpression", nExpression));
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成给定{@code inExpression}的{@code N}最大值的 BSON {@link org.bson.BsonType#ARRAY Array} ，其中{@code N}是{@code nExpression}的正整数值
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param inExpression 输入表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <InExpression> 输入表达式的类型.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/maxN/ $maxN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <InExpression, NExpression,T> BsonField maxN(
            final SFunction<T,?> fieldName, final NExpression nExpression, final InExpression inExpression) {
        return maxN(fieldName.getFieldNameLine(),nExpression,inExpression);
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成给定{@code inExpression}的{@code N}最大值的 BSON {@link org.bson.BsonType#ARRAY Array} ，其中{@code N}是{@code nExpression}的正整数值
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param inExpression 输入表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/maxN/ $maxN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression,T,R> BsonField maxN(
            final SFunction<T,?> fieldName, final NExpression nExpression, final SFunction<R,?> inExpression) {
        return maxN(fieldName.getFieldNameLine(),nExpression,inExpression.getFieldNameLineOption());
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成给定{@code inExpression}的{@code N}最大值的 BSON {@link org.bson.BsonType#ARRAY Array} ，其中{@code N}是{@code nExpression}的正整数值
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param inExpression 输入表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/maxN/ $maxN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression,T,R> BsonField maxN(
            final SFunction<T,?> fieldName, final NExpression nExpression, final SFunction<R,?>... inExpression) {
        return maxN(fieldName.getFieldNameLine(),nExpression, Arrays.stream(inExpression).map(SFunction::getFieldNameLineOption).collect(toList()));
    }

    /**
     * 返回计算字段和累加器的组合，该组合生成给定{@code inExpression}的{@code N}最大值的 BSON {@link org.bson.BsonType#ARRAY Array} ，其中{@code N}是{@code nExpression}的正整数值
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param inExpression 输入表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/maxN/ $maxN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression,T> BsonField maxN(
            final SFunction<T,?> fieldName, final NExpression nExpression, final String... inExpression) {
        return maxN(fieldName.getFieldNameLine(),nExpression, Arrays.stream(inExpression).collect(toList()));
    }

    /**
     * Gets a field name for a $group operation representing the minimum of the values of the given expression when applied to all
     * members of the group.
     *
     * @param fieldName {@link BsonField} name
     * @param expression the expression
     * @param <TExpression> the expression type
     * @return {@link BsonField}
     * @since mongodb.driver.manual reference/operator/aggregation/min/ $min
     */
    public static <TExpression> BsonField min(final String fieldName, final TExpression expression) {
        return accumulatorOperator("$min", fieldName, expression);
    }

    /**
     * 获取 $group 操作的字段名称，该字段名称表示应用于组内所有成员时给定表达式的最小值.
     *
     * @param fieldName {@link BsonField} 名称
     * @param expression 表达式
     * @param <TExpression> 表达式类型
     * @return {@link BsonField}
     * @since mongodb.driver.manual reference/operator/aggregation/min/ $min
     */
    public static <TExpression,T> BsonField min(final SFunction<T,?> fieldName, final TExpression expression) {
        return min(fieldName.getFieldNameLine(), expression);
    }

    /**
     * 获取 $group 操作的字段名称，该字段名称表示应用于组内所有成员时给定表达式的最小值.
     *
     * @param fieldName {@link BsonField} 名称
     * @param expression 表达式
     * @return {@link BsonField}
     * @since mongodb.driver.manual reference/operator/aggregation/min/ $min
     */
    public static <T,R> BsonField min(final SFunction<T,?> fieldName, final SFunction<R,?> expression) {
        return min(fieldName.getFieldNameLine(),expression.getFieldNameLineOption());
    }

    /**
     * 返回计算字段和累加器的组合，该组合产生给定{@code inExpression}的{@code N}最小值的 BSON {@link org.bson.BsonType#ARRAY Array} ，其中{@code N}是{@code nExpression}的正整数值
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param inExpression 输入表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <InExpression> 输入表达式的类型.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/minN/ $minN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <InExpression, NExpression> BsonField minN(
            final String fieldName, final NExpression nExpression, final InExpression inExpression) {
        return pickNAccumulator(notNull("fieldName", fieldName), "$minN",
                notNull("inExpression", inExpression), notNull("nExpression", nExpression));
    }

    /**
     * 返回计算字段和累加器的组合，该组合产生给定{@code inExpression}的{@code N}最小值的 BSON {@link org.bson.BsonType#ARRAY Array} ，其中{@code N}是{@code nExpression}的正整数值
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param inExpression 输入表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <InExpression> 输入表达式的类型.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/minN/ $minN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <InExpression, NExpression,T> BsonField minN(
            final SFunction<T,?> fieldName, final NExpression nExpression, final InExpression inExpression) {
        return minN(fieldName.getFieldNameLine(), nExpression, inExpression);
    }

    /**
     * 返回计算字段和累加器的组合，该组合产生给定{@code inExpression}的{@code N}最小值的 BSON {@link org.bson.BsonType#ARRAY Array} ，其中{@code N}是{@code nExpression}的正整数值
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param inExpression 输入表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/minN/ $minN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression,T,R> BsonField minN(
            final SFunction<T,?> fieldName, final NExpression nExpression, final SFunction<R,?> inExpression) {
        return minN(fieldName.getFieldNameLine(), nExpression, inExpression.getFieldNameLineOption());
    }

    /**
     * 返回计算字段和累加器的组合，该组合产生给定{@code inExpression}的{@code N}最小值的 BSON {@link org.bson.BsonType#ARRAY Array} ，其中{@code N}是{@code nExpression}的正整数值
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param inExpression 输入表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/minN/ $minN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression,T,R> BsonField minN(
            final SFunction<T,?> fieldName, final NExpression nExpression, final SFunction<R,?>... inExpression) {
        return minN(fieldName.getFieldNameLine(), nExpression, Arrays.stream(inExpression).map(SFunction::getFieldNameLineOption).collect(toList()));
    }

    /**
     * 返回计算字段和累加器的组合，该组合产生给定{@code inExpression}的{@code N}最小值的 BSON {@link org.bson.BsonType#ARRAY Array} ，其中{@code N}是{@code nExpression}的正整数值
     *
     * @param fieldName 由累加器计算的{@link BsonField}.
     * @param inExpression 输入表达式.
     * @param nExpression 限制产生的值的数量的表达式.
     * @param <NExpression> 限制表达式的类型.
     * @return 请求的 {@link BsonField}.
     * @since mongodb.driver.manual reference/operator/aggregation/minN/ $minN
     * @since 4.7
     * @since mongodb.server.release 5.2
     */
    public static <NExpression,T> BsonField minN(
            final SFunction<T,?> fieldName, final NExpression nExpression, final String... inExpression) {
        return minN(fieldName.getFieldNameLine(), nExpression, Arrays.stream(inExpression).collect(toList()));
    }

    /**
     * 获取 $group 操作的字段名称，该操作表示将表达式应用于共享相同分组键的一组文档中的每个文档所产生的所有值的数组.
     *
     * @param fieldName {@link BsonField} 名称
     * @param expression 表达式
     * @param <TExpression> 表达式类型
     * @return {@link BsonField}
     * @since mongodb.driver.manual reference/operator/aggregation/push/ $push
     */
    public static <TExpression> BsonField push(final String fieldName, final TExpression expression) {
        return accumulatorOperator("$push", fieldName, expression);
    }

    /**
     * 获取 $group 操作的字段名称，该操作表示将表达式应用于共享相同分组键的一组文档中的每个文档所产生的所有值的数组.
     *
     * @param fieldName {@link BsonField} 名称
     * @param expression 表达式
     * @param <TExpression> 表达式类型
     * @return {@link BsonField}
     * @since mongodb.driver.manual reference/operator/aggregation/push/ $push
     */
    public static <TExpression,T> BsonField push(final SFunction<T,?> fieldName, final TExpression expression) {
        return push(fieldName.getFieldNameLine(), expression);
    }

    /**
     * 获取 $group 操作的字段名称，该操作表示将表达式应用于共享相同分组键的一组文档中的每个文档所产生的所有值的数组.
     *
     * @param fieldName {@link BsonField} 名称
     * @param expression 表达式
     * @return {@link BsonField}
     * @since mongodb.driver.manual reference/operator/aggregation/push/ $push
     */
    public static <T,R> BsonField push(final SFunction<T,?> fieldName, final SFunction<R,?> expression) {
        return push(fieldName.getFieldNameLine(), expression.getFieldNameLineOption());
    }

    /**
     * 获取 $group 操作的字段名称，该操作表示将表达式应用于共享相同分组键的一组文档中的每个文档所产生的所有值的数组.
     * $push:  { item: "$item", quantity: "$quantity" }
     * @param fieldName {@link BsonField} 名称
     * @param expression 表达式
     * @return {@link BsonField}
     * @since mongodb.driver.manual reference/operator/aggregation/push/ $push
     */
    public static <T,R> BsonField push(final SFunction<T,?> fieldName, final SFunction<R,?>... expression) {
        BasicDBObject basicDBObject = new BasicDBObject();
        for (SFunction<R, ?> function : expression) {
            basicDBObject.put(function.getFieldNameLine(),function.getFieldNameLineOption());
        }
        return push(fieldName.getFieldNameLine(), basicDBObject);
    }

    /**
     * Gets a field name for a $group operation representing all unique values that results from applying the given expression to each
     * document in a group of documents that share the same group by key.
     *
     * @param fieldName {@link BsonField} name
     * @param expression the expression
     * @param <TExpression> the expression type
     * @return {@link BsonField}
     * @since mongodb.driver.manual reference/operator/aggregation/addToSet/ $addToSet
     */
    public static <TExpression> BsonField addToSet(final String fieldName, final TExpression expression) {
        return accumulatorOperator("$addToSet", fieldName, expression);
    }

    /**
     * 获取 $group 操作的字段名称，该字段表示将给定表达式应用于共享相同分组键的一组文档中的每个文档所产生的所有唯一值.
     *
     * @param fieldName {@link BsonField} 名称
     * @param expression 表达式
     * @param <TExpression> 表达式类型
     * @return {@link BsonField}
     * @since mongodb.driver.manual reference/operator/aggregation/addToSet/ $addToSet
     */
    public static <TExpression,T> BsonField addToSet(final SFunction<T,?> fieldName, final TExpression expression) {
        return addToSet(fieldName.getFieldNameLine(), expression);
    }

    /**
     * 获取 $group 操作的字段名称，该字段表示将给定表达式应用于共享相同分组键的一组文档中的每个文档所产生的所有唯一值.
     *
     * @param fieldName {@link BsonField} 名称
     * @param expression 表达式
     * @return {@link BsonField}
     * @since mongodb.driver.manual reference/operator/aggregation/addToSet/ $addToSet
     */
    public static <T,R> BsonField addToSet(final SFunction<T,?> fieldName, final SFunction<R,?> expression) {
        return addToSet(fieldName.getFieldNameLine(), expression.getFieldNameLineOption());
    }

    /**
     * 获取表示文档的{@link BsonField}合并结果的 $group 操作的字段名称。如果要合并的文档包含相同的字段名称，则结果文档中的{@link BsonField}将具有最后一个为{@link BsonField}合并的文档的值。
     *
     * @param fieldName {@link BsonField} 名称
     * @param expression 表达式
     * @param <TExpression> 表达式类型
     * @return {@link BsonField}
     * @since 4.4
     * @since mongodb.driver.manual reference/operator/aggregation/mergeObjects/ $mergeObjects
     */
    public static <TExpression> BsonField mergeObjects(final String fieldName, final TExpression expression) {
        return accumulatorOperator("$mergeObjects", fieldName, expression);
    }

    /**
     * 获取表示文档的{@link BsonField}合并结果的 $group 操作的字段名称。如果要合并的文档包含相同的字段名称，则结果文档中的{@link BsonField}将具有最后一个为{@link BsonField}合并的文档的值。
     *
     * @param fieldName {@link BsonField} 名称
     * @param expression 表达式
     * @param <TExpression> 表达式类型
     * @return {@link BsonField}
     * @since 4.4
     * @since mongodb.driver.manual reference/operator/aggregation/mergeObjects/ $mergeObjects
     */
    public static <TExpression,T> BsonField mergeObjects(final SFunction<T,?> fieldName, final TExpression expression) {
        return mergeObjects(fieldName.getFieldNameLine(), expression);
    }

    /**
     * 获取表示文档的{@link BsonField}合并结果的 $group 操作的字段名称。如果要合并的文档包含相同的字段名称，则结果文档中的{@link BsonField}将具有最后一个为{@link BsonField}合并的文档的值。
     *
     * @param fieldName {@link BsonField} 名称
     * @param expression 表达式
     * @return {@link BsonField}
     * @since 4.4
     * @since mongodb.driver.manual reference/operator/aggregation/mergeObjects/ $mergeObjects
     */
    public static <T,R> BsonField mergeObjects(final SFunction<T,?> fieldName, final SFunction<R,?> expression) {
        return mergeObjects(fieldName.getFieldNameLine(), expression.getFieldNameLineOption());
    }

    /**
     * 获取 $group 操作的字段名称，该字段表示将给定表达式应用于组内所有成员时的值的样本标准差。
     * 如果值包含您想要表示的整个数据总体，并且不希望概括更大的总体，则使用
     *
     * @param fieldName {@link BsonField} 名称
     * @param expression 表达式
     * @param <TExpression> 表达式类型
     * @return {@link BsonField}
     * @since mongodb.driver.manual reference/operator/aggregation/stdDevPop/ $stdDevPop
     * @since mongodb.server.release 3.2
     * @since 3.2
     */
    public static <TExpression> BsonField stdDevPop(final String fieldName, final TExpression expression) {
        return accumulatorOperator("$stdDevPop", fieldName, expression);
    }

    /**
     * 获取 $group 操作的字段名称，该字段表示将给定表达式应用于组内所有成员时的值的样本标准差。
     * 如果值包含您想要表示的整个数据总体，并且不希望概括更大的总体，则使用
     *
     * @param fieldName {@link BsonField} 名称
     * @param expression 表达式
     * @param <TExpression> 表达式类型
     * @return {@link BsonField}
     * @since mongodb.driver.manual reference/operator/aggregation/stdDevPop/ $stdDevPop
     * @since mongodb.server.release 3.2
     * @since 3.2
     */
    public static <TExpression,T> BsonField stdDevPop(final SFunction<T,?> fieldName, final TExpression expression) {
        return stdDevPop(fieldName.getFieldNameLine(), expression);
    }

    /**
     * 获取 $group 操作的字段名称，该字段表示将给定表达式应用于组内所有成员时的值的样本标准差。
     * 如果值包含您想要表示的整个数据总体，并且不希望概括更大的总体，则使用
     *
     * @param fieldName {@link BsonField} 名称
     * @param expression 表达式
     * @return {@link BsonField}
     * @since mongodb.driver.manual reference/operator/aggregation/stdDevPop/ $stdDevPop
     * @since mongodb.server.release 3.2
     * @since 3.2
     */
    public static <T,R> BsonField stdDevPop(final SFunction<T,?> fieldName, final SFunction<R,?> expression) {
        return stdDevPop(fieldName.getFieldNameLine(), expression.getFieldNameLineOption());
    }

    /**
     * 获取 $group 操作的字段名称，该字段表示将给定表达式应用于组内所有成员时的值的样本标准差。
     * 如果值包含数据总体的样本，并且可以通过该样本概括总体，则使用该方法
     *
     * @param fieldName {@link BsonField} 名称
     * @param expression 表达式
     * @param <TExpression> 表达式类型
     * @return {@link BsonField}
     * @since mongodb.driver.manual reference/operator/aggregation/stdDevSamp/ $stdDevSamp
     * @since mongodb.server.release 3.2
     * @since 3.2
     */
    public static <TExpression> BsonField stdDevSamp(final String fieldName, final TExpression expression) {
        return accumulatorOperator("$stdDevSamp", fieldName, expression);
    }

    /**
     * 获取 $group 操作的字段名称，该字段表示将给定表达式应用于组内所有成员时的值的样本标准差。
     * 如果值包含数据总体的样本，并且可以通过该样本概括总体，则使用该方法
     *
     * @param fieldName {@link BsonField} 名称
     * @param expression 表达式
     * @param <TExpression> 表达式类型
     * @return {@link BsonField}
     * @since mongodb.driver.manual reference/operator/aggregation/stdDevSamp/ $stdDevSamp
     * @since mongodb.server.release 3.2
     * @since 3.2
     */
    public static <TExpression,T> BsonField stdDevSamp(final SFunction<T,?> fieldName, final TExpression expression) {
        return stdDevSamp(fieldName.getFieldNameLine(), expression);
    }

    /**
     * 获取 $group 操作的字段名称，该字段表示将给定表达式应用于组内所有成员时的值的样本标准差。
     * 如果值包含数据总体的样本，并且可以通过该样本概括总体，则使用该方法
     *
     * @param fieldName {@link BsonField} 名称
     * @param expression 表达式
     * @return {@link BsonField}
     * @since mongodb.driver.manual reference/operator/aggregation/stdDevSamp/ $stdDevSamp
     * @since mongodb.server.release 3.2
     * @since 3.2
     */
    public static <T,R> BsonField stdDevSamp(final SFunction<T,?> fieldName, final SFunction<R,?> expression) {
        return stdDevSamp(fieldName.getFieldNameLine(), expression.getFieldNameLineOption());
    }

    /**
     * 创建 $accumulator 管道阶段
     *
     * @param fieldName            {@link BsonField} 名称
     * @param initFunction         用于初始化状态的函数
     * @param accumulateFunction   用于累积文档的函数
     * @param mergeFunction        用于合并两个内部状态的函数，例如在不同的分片或线程上累积的状态。它返回累加器的结果状态。
     * @return $accumulator 管道阶段
     * @since mongodb.driver.manual reference/operator/aggregation/accumulator/ $accumulator
     * @since mongodb.server.release 4.4
     * @since 4.1
     */
    public static BsonField accumulator(final String fieldName, final String initFunction, final String accumulateFunction,
                                        final String mergeFunction) {
        return accumulator(fieldName, initFunction, null, accumulateFunction, null, mergeFunction, null, "js");
    }

    /**
     * 创建 $accumulator 管道阶段
     *
     * @param fieldName            {@link BsonField} 名称
     * @param initFunction         用于初始化状态的函数
     * @param accumulateFunction   用于累积文档的函数
     * @param mergeFunction        用于合并两个内部状态的函数，例如在不同的分片或线程上累积的状态。它返回累加器的结果状态。
     * @return $accumulator 管道阶段
     * @since mongodb.driver.manual reference/operator/aggregation/accumulator/ $accumulator
     * @since mongodb.server.release 4.4
     * @since 4.1
     */
    public static <T> BsonField accumulator(final SFunction<T,?> fieldName, final String initFunction, final String accumulateFunction,
                                        final String mergeFunction) {
        return accumulator(fieldName.getFieldNameLine(), initFunction,accumulateFunction,mergeFunction);
    }

    /**
     * 创建 $accumulator 管道阶段
     *
     * @param fieldName            {@link BsonField} 名称
     * @param initFunction         用于初始化状态的函数
     * @param accumulateFunction   用于累积文档的函数
     * @param mergeFunction        用于合并两个内部状态的函数，例如在不同的分片或线程上累积的状态。它返回累加器的结果状态。
     * @param finalizeFunction     用于完成状态并返回结果的函数（可以为空）
     * @return $accumulator 管道阶段
     * @since mongodb.driver.manual reference/operator/aggregation/accumulator/ $accumulator
     * @since mongodb.server.release 4.4
     * @since 4.1
     */
    public static BsonField accumulator(final String fieldName, final String initFunction, final String accumulateFunction,
                                        final String mergeFunction, @Nullable final String finalizeFunction) {
        return accumulator(fieldName, initFunction, null, accumulateFunction, null, mergeFunction, finalizeFunction, "js");
    }

    /**
     * 创建 $accumulator 管道阶段
     *
     * @param fieldName            {@link BsonField} 名称
     * @param initFunction         用于初始化状态的函数
     * @param accumulateFunction   用于累积文档的函数
     * @param mergeFunction        用于合并两个内部状态的函数，例如在不同的分片或线程上累积的状态。它返回累加器的结果状态。
     * @param finalizeFunction     用于完成状态并返回结果的函数（可以为空）
     * @return $accumulator 管道阶段
     * @since mongodb.driver.manual reference/operator/aggregation/accumulator/ $accumulator
     * @since mongodb.server.release 4.4
     * @since 4.1
     */
    public static <T> BsonField accumulator(final SFunction<T,?> fieldName, final String initFunction, final String accumulateFunction,
                                        final String mergeFunction, @Nullable final String finalizeFunction) {
        return accumulator(fieldName.getFieldNameLine(),initFunction,accumulateFunction,mergeFunction,finalizeFunction);
    }

    /**
     * 创建 $accumulator 管道阶段
     *
     * @param fieldName            {@link BsonField} 名称
     * @param initFunction         用于初始化状态的函数
     * @param initArgs             init 函数的参数（可以为空）
     * @param accumulateFunction   用于累积文档的函数
     * @param accumulateArgs       累积函数的附加参数（可以为空）。该函数的第一个参数是“state”.
     * @param mergeFunction        用于合并两个内部状态的函数，例如在不同的分片或线程上累积的状态。它返回累加器的结果状态.
     * @param finalizeFunction     用于完成状态并返回结果的函数（可以为空）
     * @return $accumulator 管道阶段
     * @since mongodb.driver.manual reference/operator/aggregation/accumulator/ $accumulator
     * @since mongodb.server.release 4.4
     * @since 4.1
     */
    public static BsonField accumulator(final String fieldName, final String initFunction, @Nullable final List<String> initArgs,
                                        final String accumulateFunction, @Nullable final List<String> accumulateArgs,
                                        final String mergeFunction, @Nullable final String finalizeFunction) {
        return accumulator(fieldName, initFunction, initArgs, accumulateFunction, accumulateArgs, mergeFunction, finalizeFunction, "js");
    }

    /**
     * 创建 $accumulator 管道阶段
     *
     * @param fieldName            {@link BsonField} 名称
     * @param initFunction         用于初始化状态的函数
     * @param initArgs             init 函数的参数（可以为空）
     * @param accumulateFunction   用于累积文档的函数
     * @param accumulateArgs       累积函数的附加参数（可以为空）。该函数的第一个参数是“state”.
     * @param mergeFunction        用于合并两个内部状态的函数，例如在不同的分片或线程上累积的状态。它返回累加器的结果状态.
     * @param finalizeFunction     用于完成状态并返回结果的函数（可以为空）
     * @return $accumulator 管道阶段
     * @since mongodb.driver.manual reference/operator/aggregation/accumulator/ $accumulator
     * @since mongodb.server.release 4.4
     * @since 4.1
     */
    public static <T> BsonField accumulator(final SFunction<T,?> fieldName, final String initFunction, @Nullable final List<String> initArgs,
                                        final String accumulateFunction, @Nullable final List<String> accumulateArgs,
                                        final String mergeFunction, @Nullable final String finalizeFunction) {
        return accumulator(fieldName.getFieldNameLine(), initFunction, initArgs, accumulateFunction, accumulateArgs, mergeFunction, finalizeFunction);
    }

    /**
     * 创建 $accumulator 管道阶段
     *
     * @param fieldName            {@link BsonField} 名称
     * @param initFunction         用于初始化状态的函数
     * @param accumulateFunction   用于累积文档的函数
     * @param mergeFunction        用于合并两个内部状态的函数，例如在不同的分片或线程上累积的状态。它返回累加器的结果状态.
     * @param finalizeFunction     用于完成状态并返回结果的函数（可以为空）
     * @param lang                 语言说明符
     * @return $accumulator 管道阶段
     * @since mongodb.driver.manual reference/operator/aggregation/accumulator/ $accumulator
     * @since mongodb.server.release 4.4
     * @since 4.1
     */
    public static BsonField accumulator(final String fieldName, final String initFunction, final String accumulateFunction,
                                        final String mergeFunction, @Nullable final String finalizeFunction, final String lang) {
        return accumulator(fieldName, initFunction, null, accumulateFunction, null, mergeFunction, finalizeFunction, lang);
    }

    /**
     * 创建 $accumulator 管道阶段
     *
     * @param fieldName            {@link BsonField} 名称
     * @param initFunction         用于初始化状态的函数
     * @param accumulateFunction   用于累积文档的函数
     * @param mergeFunction        用于合并两个内部状态的函数，例如在不同的分片或线程上累积的状态。它返回累加器的结果状态.
     * @param finalizeFunction     用于完成状态并返回结果的函数（可以为空）
     * @param lang                 语言说明符
     * @return $accumulator 管道阶段
     * @since mongodb.driver.manual reference/operator/aggregation/accumulator/ $accumulator
     * @since mongodb.server.release 4.4
     * @since 4.1
     */
    public static <T> BsonField accumulator(final SFunction<T,?> fieldName, final String initFunction, final String accumulateFunction,
                                        final String mergeFunction, @Nullable final String finalizeFunction, final String lang) {
        return accumulator(fieldName.getFieldNameLine(), initFunction, null, accumulateFunction, null, mergeFunction, finalizeFunction, lang);
    }

    /**
     * 创建 $accumulator 管道阶段
     *
     * @param fieldName            {@link BsonField} 名称
     * @param initFunction         用于初始化状态的函数
     * @param initArgs             init 函数的参数（可以为空）
     * @param accumulateFunction   用于累积文档的函数
     * @param accumulateArgs       累积函数的附加参数（可以为空）。该函数的第一个参数是“state”.
     * @param mergeFunction        用于合并两个内部状态的函数，例如在不同的分片或线程上累积的状态。它返回累加器的结果状态.
     * @param finalizeFunction     用于完成状态并返回结果的函数（可以为空）
     * @param lang                 语言说明符
     * @return $accumulator 管道阶段
     * @since mongodb.driver.manual reference/operator/aggregation/accumulator/ $accumulator
     * @since mongodb.server.release 4.4
     * @since 4.1
     */
    public static BsonField accumulator(final String fieldName, final String initFunction, @Nullable final List<String> initArgs,
                                        final String accumulateFunction, @Nullable final List<String> accumulateArgs,
                                        final String mergeFunction, @Nullable final String finalizeFunction, final String lang) {
        BsonDocument accumulatorStage = new BsonDocument("init", new BsonString(initFunction))
                .append("initArgs", initArgs != null ? new BsonArray(initArgs.stream().map(BsonString::new).collect(toList())) : new BsonArray())
                .append("accumulate", new BsonString(accumulateFunction))
                .append("accumulateArgs", accumulateArgs != null ? new BsonArray(accumulateArgs.stream().map(BsonString::new).collect(toList())) : new BsonArray())
                .append("merge", new BsonString(mergeFunction))
                .append("lang", new BsonString(lang));
        if (finalizeFunction != null) {
            accumulatorStage.append("finalize", new BsonString(finalizeFunction));
        }
        return accumulatorOperator("$accumulator", fieldName, accumulatorStage);
    }

    /**
     * 创建 $accumulator 管道阶段
     *
     * @param fieldName            {@link BsonField} 名称
     * @param initFunction         用于初始化状态的函数
     * @param initArgs             init 函数的参数（可以为空）
     * @param accumulateFunction   用于累积文档的函数
     * @param accumulateArgs       累积函数的附加参数（可以为空）。该函数的第一个参数是“state”.
     * @param mergeFunction        用于合并两个内部状态的函数，例如在不同的分片或线程上累积的状态。它返回累加器的结果状态.
     * @param finalizeFunction     用于完成状态并返回结果的函数（可以为空）
     * @param lang                 语言说明符
     * @return $accumulator 管道阶段
     * @since mongodb.driver.manual reference/operator/aggregation/accumulator/ $accumulator
     * @since mongodb.server.release 4.4
     * @since 4.1
     */
    public static <T> BsonField accumulator(final SFunction<T,?> fieldName, final String initFunction, @Nullable final List<String> initArgs,
                                        final String accumulateFunction, @Nullable final List<String> accumulateArgs,
                                        final String mergeFunction, @Nullable final String finalizeFunction, final String lang) {
        return accumulator(fieldName.getFieldNameLine(), initFunction, initArgs, accumulateFunction, accumulateArgs, mergeFunction, finalizeFunction, lang);
    }

    private static <TExpression> BsonField accumulatorOperator(final String name, final String fieldName, final TExpression expression) {
        return new BsonField(fieldName, new SimpleExpression<>(name, expression));
    }

    private static <InExpression, NExpression> BsonField pickNAccumulator(
            final String fieldName, final String accumulatorName, final InExpression inExpression, final NExpression nExpression) {
        return new BsonField(fieldName, new Document(accumulatorName, new Document("input", inExpression).append("n", nExpression)));
    }

    private static <OutExpression> BsonField sortingPickAccumulator(
            final String fieldName, final String accumulatorName, final Bson sort, final OutExpression outExpression) {
        return new BsonField(fieldName, new Document(accumulatorName, new Document("sortBy", sort).append("output", outExpression)));
    }

    private static <OutExpression, NExpression> BsonField sortingPickNAccumulator(
            final String fieldName, final String accumulatorName,
            final Bson sort, final OutExpression outExpression, final NExpression nExpression) {
        return new BsonField(fieldName, new Document(accumulatorName, new Document("sortBy", sort)
                .append("output", outExpression)
                .append("n", nExpression)));
    }

    private Accumulators() {
    }
    
}
