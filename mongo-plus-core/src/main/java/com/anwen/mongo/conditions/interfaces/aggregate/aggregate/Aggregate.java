package com.anwen.mongo.conditions.interfaces.aggregate.aggregate;

import com.anwen.mongo.conditions.aggregate.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.interfaces.aggregate.Project;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.project.Projections;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.model.aggregate.Field;
import com.anwen.mongo.support.SFunction;
import com.mongodb.client.model.*;
import com.mongodb.lang.Nullable;
import org.bson.conversions.Bson;

import java.util.Collection;
import java.util.List;

import static java.util.Arrays.asList;

public interface Aggregate<Children> extends Project<Children> {

    /**
     * 获取管道列表
     * @return {@link java.util.List<org.bson.conversions.Bson>}
     * @author anwen
     * @date 2024/6/11 下午8:18
     */
    List<Bson> getAggregateConditionList();

    /* $addFields阶段 start */

    /**
     * 向嵌入文档或文档中加入一个新字段
     * $addFields: {"specs": "unleaded"}
     * $addFields: {"specs.fuel_type": "unleaded"}
     * @param field 字段
     * @param value 值
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:37
     */
    Children addFields(final String field,final String value);

    /**
     * 向嵌套文档或文档中加入一个新字段
     * $addFields: {"specs": "unleaded"}
     * $addFields: {"specs.fuel_type": "unleaded"}
     * @param field 字段
     * @param value 值
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:37
     */
    <T,R> Children addFields(final SFunction<T,R> field,final String value);

    /**
     * 向嵌套文档或文档中加入一个新字段
     * $addFields: {"specs": "unleaded"}
     * $addFields: {"specs.fuel_type": "unleaded"}
     * @param value 值
     * @param field 字段，嵌套文档请按照顺序传入，中间会自动拼接.
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:37
     */
    @SuppressWarnings("unchecked")
    <T,R> Children addFields(final String value,final SFunction<T,R>... field);

    /**
     * $addFields阶段，指定现有字段，覆盖原字段
     * @param field 字段
     * @param value 值
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:31
     */
    <T,R> Children addFields(final SFunction<T,R> field,final Object value);

    /**
     * $addFields阶段，向数组中添加元素
     * @param field 数组字段
     * @param value 需要向数组中添加的值
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:44
     */
    <T,R> Children addFields(final SFunction<T,R> field, final Collection<?> value);

    /**
     * $addFields阶段，向数组中添加元素
     * @param field 数组
     * @param value 需要向数组中添加的值
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:44
     */
    Children addFields(final String field, final Collection<?> value);

    /**
     * $addFields阶段
     * @param fields 多个Field
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:26
     */
    Children addFields(final Field<?>... fields);

    /**
     * $addFields阶段
     * @param fields 多个Field
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:26
     */
    Children addFields(final List<Field<?>> fields);

    /**
     * $addFields阶段，如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson，或使用${@link Field}
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:47
     */
    Children addFields(final Bson bson);

    /* $addFields阶段 end */

    /* =============================================================================== */

    /* $set阶段 start */

    /**
     * 向嵌入文档或文档中加入一个新字段
     * $set: {"specs": "unleaded"}
     * $set: {"specs.fuel_type": "unleaded"}
     * @param field 字段
     * @param value 值
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:37
     */
    Children set(final String field,final String value);

    /**
     * 向嵌套文档或文档中加入一个新字段
     * $set: {"specs": "unleaded"}
     * $set: {"specs.fuel_type": "unleaded"}
     * @param field 字段
     * @param value 值
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:37
     */
    <T,R> Children set(final SFunction<T,R> field,final String value);

    /**
     * 向嵌套文档或文档中加入一个新字段
     * $set: {"specs": "unleaded"}
     * $set: {"specs.fuel_type": "unleaded"}
     * @param value 值
     * @param field 字段，嵌套文档请按照顺序传入，中间会自动拼接.
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:37
     */
    @SuppressWarnings("unchecked")
    <T,R> Children set(final String value,final SFunction<T,R>... field);

    /**
     * $set阶段，指定现有字段，覆盖原字段
     * @param field 字段
     * @param value 值
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:31
     */
    <T,R> Children set(final SFunction<T,R> field,final Object value);

    /**
     * $set阶段，向数组中添加元素
     * @param field 数组字段
     * @param value 需要向数组中添加的值
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:44
     */
    <T,R> Children set(final SFunction<T,R> field, final Collection<?> value);

    /**
     * $set阶段，向数组中添加元素
     * @param field 数组
     * @param value 需要向数组中添加的值
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:44
     */
    Children set(final String field, final Collection<?> value);

    /**
     * $set阶段
     * @param fields 多个Field
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:26
     */
    Children set(final Field<?>... fields);

    /**
     * $set阶段
     * @param fields 多个Field
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:26
     */
    Children set(final List<Field<?>> fields);

    /**
     * $set阶段，如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson，或使用${@link Field}
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:47
     */
    Children set(final Bson bson);

    /* $set阶段 end */

    /* =============================================================================== */

    /* $bucket阶段 start */

    /**
     * $bucket阶段
     * @param groupBy 分组字段
     * @param boundaries 桶边界
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午5:07
     */
    <T,R,Boundary> Children bucket(final SFunction<T,R> groupBy,final List<Boundary> boundaries);

    /**
     * $bucket阶段
     * @param groupBy 分组字段
     * @param boundaries 桶边界
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午5:07
     */
    <Boundary> Children bucket(final Object groupBy,final List<Boundary> boundaries);

    /**
     * $bucket阶段
     * @param groupBy 分组字段
     * @param boundaries 桶边界
     * @param options 可选值，其中包含default和output
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午5:08
     */
    <T,R,Boundary> Children bucket(final SFunction<T,R> groupBy, final List<Boundary> boundaries, BucketOptions options);

    /**
     * $bucket阶段
     * @param groupBy 分组字段
     * @param boundaries 桶边界
     * @param options 可选值，其中包含default和output
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午5:08
     */
    <Boundary> Children bucket(final Object groupBy, final List<Boundary> boundaries, BucketOptions options);

    /**
     * $bucket阶段，如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:47
     */
    Children bucket(final Bson bson);

    /* $bucket阶段 end */

    /* =============================================================================== */

    /* $bucketAuto阶段 start */

    /**
     * $bucketAuto阶段
     * @param groupBy 分组字段
     * @param buckets 桶的数量
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午5:15
     */
    <T,R> Children bucketAuto(final SFunction<T,R> groupBy,final Integer buckets);

    /**
     * $bucketAuto阶段
     * @param groupBy 分组字段
     * @param buckets 桶的数量
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午5:15
     */
    Children bucketAuto(final Object groupBy,final Integer buckets);

    /**
     * $bucketAuto阶段
     * @param groupBy 分组字段
     * @param buckets 桶的数量
     * @param options 可选值，其中包含output和granularity
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午5:18
     */
    <T,R> Children bucketAuto(final SFunction<T,R> groupBy, final Integer buckets, BucketAutoOptions options);

    /**
     * $bucketAuto阶段
     * @param groupBy 分组字段
     * @param buckets 桶的数量
     * @param options 可选值，其中包含output和granularity
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午5:18
     */
    Children bucketAuto(final Object groupBy, final Integer buckets, BucketAutoOptions options);

    /**
     * $bucketAuto阶段，如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:47
     */
    Children bucketAuto(final Bson bson);

    /* $bucketAuto阶段 end */

    /* =============================================================================== */

    /* $count阶段 start */

    /**
     * $count阶段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午5:45
     */
    Children count();

    /**
     * $count阶段
     * @param field 输出字段名
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午5:45
     */
    Children count(final String field);

    /**
     * $count阶段
     * @param field 输出字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午5:45
     */
    <T,R> Children count(final SFunction<T,R> field);

    /* $count阶段 end */

    /* =============================================================================== */

    /* $match阶段 start */

    /**
     * $match阶段
     * @param queryChainWrapper 条件
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午2:22
     */
    Children match(final QueryChainWrapper<?, ?> queryChainWrapper);

    /**
     * $match阶段，如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson
     * @param bson bson
     * @author anwen
     * @date 2024/6/10 下午3:54
     */
    Children match(final Bson bson);

    /* $match阶段 end */

    /* =============================================================================== */

    /* $project阶段 start */

    /**
     * $project阶段，如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson
     * 如：使用{@link Projections}进行构建，基于MongoDB驱动提供，进行封装，支持lambda形式
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午2:20
     */
    Children project(final Bson bson);

    /* $project阶段 end */

    /* =============================================================================== */

    /* $sort阶段 start */

    /**
     * $sort阶段
     * @param field 字段
     * @param value 值
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午5:33
     */
    Children sort(final String field, final Integer value);

    /**
     * $sort阶段
     * @param field 字段
     * @param value 值
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午5:33
     */
    <T,R> Children sort(final SFunction<T,R> field,final Integer value);

    /**
     * $sort阶段，按照指定字段升序排序
     * @param field 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午5:35
     */
    <T,R> Children sortAsc(final SFunction<T,R> field);

    /**
     * $sort阶段，按照指定字段升序排序
     * @param field 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午5:35
     */
    Children sortAsc(final String field);

    /**
     * $sort阶段，按照指定字段升序排序
     * @param field 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午5:35
     */
    <T,R> Children sortAsc(final SFunction<T, R>... field);

    /**
     * $sort阶段，按照指定字段升序排序
     * @param field 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午5:35
     */
    Children sortAsc(final String... field);

    /**
     * $sort阶段，按照指定字段升序排序
     * @param field 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午5:35
     */
    <T,R> Children sortAscLambda(final List<SFunction<T,R>> field);

    /**
     * $sort阶段，按照指定字段升序排序
     * @param field 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午5:35
     */
    Children sortAsc(final List<String> field);

    /**
     * $sort阶段，按照指定字段降序排序
     * @param field 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午5:35
     */
    <T,R> Children sortDesc(final SFunction<T,R> field);

    /**
     * $sort阶段，按照指定字段降序排序
     * @param field 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午5:35
     */
    Children sortDesc(final String field);

    /**
     * $sort阶段，按照指定字段降序排序
     * @param field 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午5:35
     */
    <T,R> Children sortDesc(final SFunction<T, R>... field);

    /**
     * $sort阶段，按照指定字段降序排序
     * @param field 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午5:35
     */
    Children sortDesc(final String... field);

    /**
     * $sort阶段，按照指定字段降序排序
     * @param field 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午5:35
     */
    <T,R> Children sortDescLambda(final List<SFunction<T,R>> field);

    /**
     * $sort阶段，按照指定字段降序排序
     * @param field 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午5:35
     */
    Children sortDesc(final List<String> field);

    /**
     * 为给定字段上的文本分数元投影创建排序规范
     * @param field 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午6:31
     */
    Children metaTextScore(final String field);

    /**
     * 为给定字段上的文本分数元投影创建排序规范
     * @param field 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午6:31
     */
    <T,R> Children metaTextScore(final SFunction<T,R> field);

    /**
     * $sort阶段，如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午5:32
     */
    Children sort(final Bson bson);

    /* $sort阶段 end */

    /* =============================================================================== */

    /* $sortByCount阶段 start */

    /**
     * $sortByCount阶段
     * @param field 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午6:36
     */
    Children sortByCount(final String field);

    /**
     * $sortByCount阶段
     * @param field 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午6:36
     */
    <T,R> Children sortByCount(final SFunction<T,R> field);

    /* $sortByCount阶段 end */

    /* =============================================================================== */

    /* $skip阶段 start */

    /**
     * $skip阶段，当前页
     * @param skip 当前页
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午6:39
     */
    Children skip(final long skip);

    /**
     * $skip阶段，当前页
     * @param skip 当前页
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午6:39
     */
    Children skip(final int skip);

    /* $skip阶段 end */

    /* =============================================================================== */

    /* $limit阶段 start */

    /**
     * $limit阶段 每页显示行数
     * @param limit 每页显示行数
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午6:40
     */
    Children limit(final long limit);

    /**
     * $limit阶段 每页显示行数
     * @param limit 每页显示行数
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午6:40
     */
    Children limit(final int limit);

    /* $limit阶段 end */

    /* =============================================================================== */

    /* $lookup阶段 start */

    /**
     * $lookup阶段
     * @param from 目标集合名称
     * @param localField 当前集合用于关联的字段
     * @param foreignField 指定目标集合用于关联的字段
     * @param as 输出结果中保存关联值的字段名
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午7:05
     */
    Children lookup(final String from,final String localField,final String foreignField,final String as);

    /**
     * $lookup阶段
     * @param from 目标集合名称
     * @param localField 当前集合用于关联的字段
     * @param foreignField 指定目标集合用于关联的字段
     * @param as 输出结果中保存关联值的字段名
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午7:05
     */
    <T,R> Children lookup(final String from,final SFunction<T,R> localField,final SFunction<T,R> foreignField, final String as);

    /**
     * $lookup阶段
     * @param from 目标集合名称
     * @param localField 当前集合用于关联的字段
     * @param foreignField 指定目标集合用于关联的字段
     * @param as 输出结果中保存关联值的字段名
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午7:05
     */
    <T,R> Children lookup(final String from,final SFunction<T,R> localField,final String foreignField,final String as);

    /**
     * $lookup阶段
     * @param from 目标集合名称
     * @param localField 当前集合用于关联的字段
     * @param foreignField 指定目标集合用于关联的字段
     * @param as 输出结果中保存关联值的字段名
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午7:05
     */
    <T,R> Children lookup(final String from,final String localField,final SFunction<T,R> foreignField,final String as);

    /**
     * $lookup阶段
     * @param from 目标集合名称
     * @param letList 在管道字段阶段使用的变量
     * @param aggregate 在连接集合上运行的管道
     * @param as 输出结果中保存关联值的字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
     */
    <TExpression> Children lookup(final String from, final List<Variable<TExpression>> letList,
                                  final AggregateChainWrapper<?> aggregate, final String as);

    /**
     * $lookup阶段
     * @param from 目标集合名称
     * @param aggregate 在连接集合上运行的管道
     * @param as 输出结果中保存关联值的字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/12 21:07
     */
    Children lookup(final String from, final AggregateChainWrapper<?> aggregate, final String as);

    /**
     * $lookup阶段,如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午6:45
     */
    Children lookup(final Bson bson);

    /* $lookup阶段 end */

    /* =============================================================================== */

    /* $facet阶段 start */

    /**
     * $facet阶段
     * @param name facet名称
     * @param pipeline facet管道
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午8:13
     */
    Children facet(final String name, final Bson... pipeline);

    /**
     * $facet阶段
     * @param name facet名称
     * @param pipeline facet管道
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午8:13
     */
    Children facet(final String name, final List<? extends Bson> pipeline);

    /**
     * $facet阶段
     * @param name facet名称
     * @param aggregate facet管道
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午8:14
     */
    Children facet(final String name, final Aggregate<?> aggregate);

    /**
     * $facet阶段
     * @param facets facets，可以使用{@link com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Facet}进行构建
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午8:16
     */
    Children facet(final Facet... facets);

    /**
     * $facet阶段
     * @param facets facets，可以使用{@link com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Facet}进行构建
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午8:16
     */
    Children facet(final List<Facet> facets);

    /**
     * $facet阶段,如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午8:03
     */
    Children facet(final Bson bson);

    /* $facet阶段 end */

    /* =============================================================================== */

    /* $graphLookup阶段 start */

    /**
     * $graphLookup阶段
     * @param from 要查询的集合
     * @param startWith 启动图形查找的表达式
     * @param connectFromField 来自字段
     * @param connectToField 目标字段
     * @param as 输出文档中的字段名称
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午8:30
     */
    Children graphLookup(final String from, final Object startWith, final String connectFromField,
                         final String connectToField, final String as);

    /**
     * $graphLookup阶段
     * @param from 要查询的集合
     * @param startWith 启动图形查找的表达式
     * @param connectFromField 来自字段
     * @param connectToField 目标字段
     * @param as 输出文档中的字段名称
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午8:30
     */
    <T,R> Children graphLookup(final String from, final SFunction<T,R> startWith, final SFunction<T,R> connectFromField,
                         final SFunction<T,R> connectToField, final String as);

    /**
     * $graphLookup阶段
     * @param from 要查询的集合
     * @param startWith 启动图形查找的表达式
     * @param connectFromField 来自字段
     * @param connectToField 目标字段
     * @param as 输出文档中的字段名称
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午8:30
     */
    <T,R> Children graphLookup(final String from, final Object startWith, final SFunction<T,R> connectFromField,
                               final SFunction<T,R> connectToField, final String as);

    /**
     * $graphLookup阶段
     * @param from 要查询的集合
     * @param startWith 启动图形查找的表达式
     * @param connectFromField 来自字段
     * @param connectToField 目标字段
     * @param as 输出文档中的字段名称
     * @param options 查找选项:
     * <div>
     *      <p>maxDepth（指定最大递归深度的非负整）</p>
     *      <p>depthField（要添加到搜索路径中每个遍历文档的字段的名称）</p>
     *      <p>restrictSearchWithMatch（指定递归搜索的附加条件的文档，不能是表达式，比如不能是{@code { lastName: { $ne: "$lastName" } }}）</p>
     * </div>
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午8:30
     */
    Children graphLookup(final String from, final Object startWith, final String connectFromField,
                         final String connectToField, final String as, final GraphLookupOptions options);

    /**
     * $graphLookup阶段
     * @param from 要查询的集合
     * @param startWith 启动图形查找的表达式
     * @param connectFromField 来自字段
     * @param connectToField 目标字段
     * @param as 输出文档中的字段名称
     * @param options 查找选项:
     * <div>
     *      <p>maxDepth（指定最大递归深度的非负整）</p>
     *      <p>depthField（要添加到搜索路径中每个遍历文档的字段的名称）</p>
     *      <p>restrictSearchWithMatch（指定递归搜索的附加条件的文档，不能是表达式，比如不能是{@code { lastName: { $ne: "$lastName" } }}）</p>
     * </div>
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午8:30
     */
    <T,R> Children graphLookup(final String from, final SFunction<T,R> startWith, final SFunction<T,R> connectFromField,
                               final SFunction<T,R> connectToField, final String as, final GraphLookupOptions options);

    /**
     * $graphLookup阶段
     * @param from 要查询的集合
     * @param startWith 启动图形查找的表达式
     * @param connectFromField 来自字段
     * @param connectToField 目标字段
     * @param as 输出文档中的字段名称
     * @param options 查找选项:
     * <div>
     *      <p>maxDepth（指定最大递归深度的非负整）</p>
     *      <p>depthField（要添加到搜索路径中每个遍历文档的字段的名称）</p>
     *      <p>restrictSearchWithMatch（指定递归搜索的附加条件的文档，不能是表达式，比如不能是{@code { lastName: { $ne: "$lastName" } }}）</p>
     * </div>
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午8:30
     */
    <T,R> Children graphLookup(final String from, final Object startWith, final SFunction<T,R> connectFromField,
                               final SFunction<T,R> connectToField, final String as, final GraphLookupOptions options);

    /**
     * $graphLookup阶段,如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午8:26
     */
    Children graphLookup(final Bson bson);

    /* $graphLookup阶段 end */

    /* =============================================================================== */

    /* $group阶段 start */

    /**
     * $group阶段，只有一个字段参数的情况，如{@code $group : { _id : "$item" }}
     * @param _id group的_id表达式
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午9:12
     */
    Children group(final String _id);

    /**
     * $group阶段，只有一个字段参数的情况，如{@code $group : { _id : "$item" }}
     * @param _id group的_id表达式
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午9:12
     */
    <T,R> Children group(final SFunction<T,R> _id);

    /**
     * $group阶段
     * @param id group的_id表达式，可以为null
     * @param fieldAccumulators 零个或多个字段累加器对
     * @return {@link org.bson.conversions.Bson}
     * @author anwen
     * @date 2024/6/11 下午9:06
     */
    <TExpression> Children group(@Nullable final TExpression id, final BsonField... fieldAccumulators);

    /**
     * $group阶段
     * @param id group的_id表达式，可以为null
     * @param fieldAccumulators 零个或多个字段累加器对
     * @return {@link org.bson.conversions.Bson}
     * @author anwen
     * @date 2024/6/11 下午9:07
     */
    <TExpression> Children group(@Nullable final TExpression id, final List<BsonField> fieldAccumulators);

    /**
     * $group阶段,如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午9:04
     */
    Children group(final Bson bson);

    /**
     * 如果缺少管道，请使用该方法构建
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午5:49
     */
    Children custom(final Bson bson);

}
