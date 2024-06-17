package com.anwen.mongo.conditions.interfaces.aggregate.aggregate;

import com.anwen.mongo.conditions.aggregate.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Accumulators;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Project;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.project.Projections;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.model.aggregate.Field;
import com.anwen.mongo.support.SFunction;
import com.mongodb.MongoNamespace;
import com.mongodb.client.model.*;
import com.mongodb.client.model.densify.DensifyOptions;
import com.mongodb.client.model.densify.DensifyRange;
import com.mongodb.lang.Nullable;
import org.bson.conversions.Bson;

import java.util.Collection;
import java.util.List;

import static com.mongodb.assertions.Assertions.notNull;
import static com.mongodb.client.model.densify.DensifyOptions.densifyOptions;
import static com.mongodb.internal.Iterables.concat;

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
    <T> Children addFields(final SFunction<T,?> field,final String value);

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
    <T> Children addFields(final String value,final SFunction<T,?>... field);

    /**
     * $addFields阶段，指定现有字段，覆盖原字段
     * @param field 字段
     * @param value 值
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:31
     */
    <T> Children addFields(final SFunction<T,?> field,final Object value);

    /**
     * $addFields阶段，向数组中添加元素
     * @param field 数组字段
     * @param value 需要向数组中添加的值
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:44
     */
    <T> Children addFields(final SFunction<T,?> field, final Collection<?> value);

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
    <T> Children set(final SFunction<T,?> field,final String value);

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
    <T> Children set(final String value,final SFunction<T,?>... field);

    /**
     * $set阶段，指定现有字段，覆盖原字段
     * @param field 字段
     * @param value 值
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:31
     */
    <T> Children set(final SFunction<T,?> field,final Object value);

    /**
     * $set阶段，向数组中添加元素
     * @param field 数组字段
     * @param value 需要向数组中添加的值
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午3:44
     */
    <T> Children set(final SFunction<T,?> field, final Collection<?> value);

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
    <Boundary,T> Children bucket(final SFunction<T,?> groupBy,final List<Boundary> boundaries);

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
    <Boundary,T> Children bucket(final SFunction<T,?> groupBy, final List<Boundary> boundaries, BucketOptions options);

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
    <T> Children bucketAuto(final SFunction<T,?> groupBy,final Integer buckets);

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
    <T> Children bucketAuto(final SFunction<T,?> groupBy, final Integer buckets, BucketAutoOptions options);

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
    <T> Children count(final SFunction<T,?> field);

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
    <T> Children sort(final SFunction<T,?> field,final Integer value);

    /**
     * $sort阶段，按照指定字段升序排序
     * @param field 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/11 下午5:35
     */
    <T> Children sortAsc(final SFunction<T,?> field);

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
    <T> Children sortAsc(final SFunction<T, ?>... field);

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
    <T> Children sortAscLambda(final List<SFunction<T,?>> field);

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
    <T> Children sortDesc(final SFunction<T,?> field);

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
    <T> Children sortDesc(final SFunction<T, ?>... field);

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
    <T> Children sortDescLambda(final List<SFunction<T,?>> field);

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
    <T> Children metaTextScore(final SFunction<T,?> field);

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
    <T> Children sortByCount(final SFunction<T,?> field);

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
    <T,R> Children lookup(final String from,final SFunction<T,?> localField,final SFunction<R,?> foreignField, final String as);

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
    <T> Children lookup(final String from,final SFunction<T,?> localField,final String foreignField,final String as);

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
    <T> Children lookup(final String from,final String localField,final SFunction<T,?> foreignField,final String as);

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
    <T,R,U> Children graphLookup(final String from, final SFunction<T,?> startWith, final SFunction<R,?> connectFromField,
                         final SFunction<U,?> connectToField, final String as);

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
    <T,R> Children graphLookup(final String from, final Object startWith, final SFunction<T,?> connectFromField,
                               final SFunction<R,?> connectToField, final String as);

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
    <T,R,U> Children graphLookup(final String from, final SFunction<T,?> startWith, final SFunction<R,?> connectFromField,
                               final SFunction<U,?> connectToField, final String as, final GraphLookupOptions options);

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
    <T,R> Children graphLookup(final String from, final Object startWith, final SFunction<T,?> connectFromField,
                               final SFunction<R,?> connectToField, final String as, final GraphLookupOptions options);

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
    <T> Children group(final SFunction<T,?> _id);

    /**
     * $group阶段
     * @param id group的_id表达式，可以为null
     * @param fieldAccumulators 零个或多个字段累加器对，使用{@link Accumulators}构建
     * @return {@link org.bson.conversions.Bson}
     * @author anwen
     * @date 2024/6/11 下午9:06
     */
    <TExpression> Children group(@Nullable final TExpression id, final BsonField... fieldAccumulators);

    /**
     * $group阶段
     * @param id group的_id表达式，可以为null
     * @param fieldAccumulators 零个或多个字段累加器对，使用{@link Accumulators}构建
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

    /* $group阶段 end */

    /* =============================================================================== */

    /* $unionWith阶段 start */

    /**
     * $unionWith阶段
     * @param collectionName 要执行合并的同一数据库中的集合的名称
     * @param aggregate 应用于输入文档的聚合管道
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午8:44
     */
    Children unionWith(final String collectionName,final Aggregate<?> aggregate);

    /**
     * $unionWith阶段
     * @param collectionName 要执行合并的同一数据库中的集合的名称
     * @param aggregate 应用于输入文档的聚合管道
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午8:44
     */
    Children unionWith(final String collectionName,final List<? extends Bson> aggregate);

    /**
     * $unionWith阶段
     * @param collection 集合名称，取用类名，如有@CollectionName注解，则取用注解值
     * @param aggregate 应用于输入文档的聚合管道
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午8:44
     */
    Children unionWith(final Class<?> collection,final Aggregate<?> aggregate);

    /**
     * $unionWith阶段
     * @param collection 集合名称，取用类名，如有@CollectionName注解，则取用注解值
     * @param aggregate 应用于输入文档的聚合管道
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午8:44
     */
    Children unionWith(final Class<?> collection,final List<? extends Bson> aggregate);

    /**
     * $unionWith阶段，如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午8:41
     */
    Children unionWith(final Bson bson);

    /* $unionWith阶段 end */

    /* =============================================================================== */

    /* $unwind阶段 start */

    /**
     * $unwind阶段
     * @param fieldName 该字段名称必须以'$'符号为前缀
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午9:02
     */
    Children unwind(final String fieldName);

    /**
     * $unwind阶段
     * @param fieldName 字段名称
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午9:02
     */
    <T> Children unwind(final SFunction<T,?> fieldName);

    /**
     * $unwind阶段
     * @param fieldName 该字段名称必须以'$'符号为前缀
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午9:02
     */
    Children unwind(final String fieldName, final com.anwen.mongo.conditions.interfaces.aggregate.pipeline.UnwindOptions unwindOptions);

    /**
     * $unwind阶段
     * @param fieldName 字段名称
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午9:02
     */
    <T> Children unwind(final SFunction<T,?> fieldName,final com.anwen.mongo.conditions.interfaces.aggregate.pipeline.UnwindOptions unwindOptions);

    /**
     * $unwind阶段，如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午9:02
     */
    Children unwind(final Bson bson);

    /* $unwind阶段 end */

    /* =============================================================================== */

    /* $out阶段 start */

    /**
     * $out阶段
     * @param collectionName 集合名称
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午9:19
     */
    Children out(final String collectionName);

    /**
     * $out阶段
     * @param collection 集合名称
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午9:19
     */
    Children out(final Class<?> collection);

    /**
     * $out
     * @param databaseName 数据库名称
     * @param collectionName 集合名称
     * @return {@link Bson}
     * @author anwen
     * @date 2024/6/16 下午9:20
     */
    Children out(final String databaseName, final String collectionName);

    /**
     * $out阶段,如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午9:18
     */
    Children out(final Bson bson);

    /* $out阶段 end */

    /* =============================================================================== */

    /* $merge阶段 start */

    /**
     * $merge阶段
     * @param collectionName 要合并的集合的名称
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午9:49
     */
    Children merge(final String collectionName);

    /**
     * $merge阶段
     * @param collection 集合
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午9:49
     */
    Children merge(final Class<?> collection);

    /**
     * $merge阶段
     * @param namespace 要合并到的命名空间
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午9:50
     */
    Children merge(final MongoNamespace namespace);

    /**
     * $merge阶段
     * @param collectionName 要合并的集合的名称
     * @param options 合并选项
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午9:51
     */
    Children merge(final String collectionName, final MergeOptions options);

    /**
     * $merge阶段
     * @param collection 要合并的集合
     * @param options 合并选项
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午9:51
     */
    Children merge(final Class<?> collection, final MergeOptions options);

    /**
     * $merge阶段
     * @param namespace 要合并到的命名空间
     * @param options 合并选项
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午9:52
     */
    Children merge(final MongoNamespace namespace, final MergeOptions options);

    /**
     * $merge阶段，如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午9:47
     */
    Children merge(final Bson bson);

    /* $out阶段 end */

    /* =============================================================================== */

    /* replaceRoot阶段 */

    /**
     * $replaceRoot阶段
     * @param fieldName 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/17 下午10:39
     */
    <TExpression> Children replaceRoot(final TExpression fieldName);

    /**
     * $replaceRoot阶段
     * @param fieldName 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/17 下午10:39
     */
    <T> Children replaceRoot(final SFunction<T,?> fieldName);

    /**
     * $replaceRoot阶段，如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/16 下午9:56
     */
    Children replaceRoot(final Bson bson);

    /* $replaceRoot阶段 end */

    /* =============================================================================== */

    /* $replaceWith阶段 start*/

    /**
     * $replaceWith阶段
     * @param fieldName 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/17 下午10:42
     */
    <TExpression> Children replaceWith(final TExpression fieldName);

    /**
     * $replaceWith阶段
     * @param fieldName 字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/17 下午10:39
     */
    <T> Children replaceWith(final SFunction<T,?> fieldName);

    /**
     * $replaceWith阶段，如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/17 下午10:42
     */
    Children replaceWith(final Bson bson);

    /* $replaceWith阶段 end */

    /* =============================================================================== */

    /* $sample阶段 start */

    /**
     * $sample阶段
     * @param size 指定数量
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/17 下午10:46
     */
    Children sample(final int size);

    /**
     * $sample阶段
     * @param size 指定数量
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/17 下午10:46
     */
    Children sample(final long size);

    /**
     * $sample阶段，如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/17 下午10:45
     */
    Children sample(final Bson bson);

    /* $sample阶段 end */

    /* =============================================================================== */

    /* $setWindowFields阶段 start */

    /**
     * $setWindowFields阶段，可以使用{@link WindowOutputFields}构建WindowOutputField
     * 创建一个 {@code $setWindowFields} 管道阶段，允许使用窗口运算符.
     * 此阶段对输入文档进行分区，类似于 {@link #group(Object, List) $group} 管道阶段,可选择对它们进行排序，
     * 通过计算每个指定的 {@linkplain Window windows} 上的窗口函数来计算文档中的字段函数，并输出文档。
     * 与{@code $group}管道阶段的重要区别在于，属于同一分区或窗口的文档不会折叠成单个文档.
     *
     * @param partitionBy 可选的数据分区，如 {@link #group(Object, List)} 中的 {@code id} 指定.
     *                    如果{@code null}，则所有文档属于同一分区.
     * @param sortBy 排序依据的字段。语法与 {@link #sort(Bson)} 中的 {@code sort} 相同（请参阅 {@link com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Sorts}）.
     *               某些函数需要排序，某些窗口可能需要排序（有关更多详细信息，请参阅{@link Windows}）.
     *               排序仅用于计算窗口函数，并不保证输出文档的排序.
     * @param output {@linkplain WindowOutputField 窗口计算}.
     * @param moreOutput 更多{@linkplain WindowOutputField 窗口计算}.
     * @param <TExpression> {@code partitionBy} 表达式类型.
     * @return {@code $setWindowFields} 管道阶段.
     * @author anwen
     * @date 2024/6/17 下午11:46
     */
    <TExpression> Children setWindowFields(@Nullable final TExpression partitionBy, @Nullable final Bson sortBy,
                                                     final WindowOutputField output, final WindowOutputField... moreOutput);

    /**
     * $setWindowFields阶段，可以使用{@link WindowOutputFields}构建WindowOutputField
     * 创建一个 {@code $setWindowFields} 管道阶段，允许使用窗口运算符.
     * 此阶段对输入文档进行分区，类似于 {@link #group(Object, List) $group} 管道阶段,
     * 可选择对它们进行排序, 通过计算每个指定的 {@linkplain Window windows} 上的窗口函数来计算文档中的字段功能,
     * 并输出文档。与 {@code $group} 管道阶段的重要区别在于,属于同一分区或窗口的文档不会折叠成一个文档.
     *
     * @param partitionBy 数据的可选分区指定为 {@link #group(Object, List)} 中的 {@code id}。如果{@code null}，则所有文档属于同一个分区.
     * @param sortBy 排序依据的字段。语法与 {@link #sort(Bson)} 中的 {@code sort} 相同（请参阅 {@link com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Sorts}）.
     *               某些函数需要排序，某些窗口可能需要排序（有关更多详细信息，请参阅{@link Windows}）.
     *               排序仅用于计算窗口函数，并不保证输出文档的排序.
     * @param output {@linkplain WindowOutputField 窗口计算}的列表.
     * 指定空列表不是错误，但生成的阶段不会执行任何有用的操作.
     * @param <TExpression> {@code partitionBy} 表达式类型.
     * @return {@code $setWindowFields} 管道阶段.
     * @author anwen
     * @date 2024/6/17 下午11:46
     */
    <TExpression> Children setWindowFields(@Nullable final TExpression partitionBy, @Nullable final Bson sortBy,
                                                     final Iterable<? extends WindowOutputField> output);

    /**
     * $setWindowFields阶段，如果MongoPlus封装的条件未满足该阶段的需求，请自行构建Bson
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/17 下午11:41
     */
    Children setWindowFields(Bson bson);

    /* $setWindowFields阶段 end */

    /* =============================================================================== */

    /* $densify阶段 start */

    /**
     * $densify阶段
     * @param field 字段
     * @param range 范围 指定如何密集化数据的对象
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/18 上午12:03
     */
    Children densify(final String field, final DensifyRange range);

    /**
     * $densify阶段
     * @param field 字段
     * @param range 范围 指定如何密集化数据的对象
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/18 上午12:04
     */
    <T> Children densify(final SFunction<T,?> field, final DensifyRange range);

    /**
     * $desify阶段
     * @param field 字段
     * @param range 范围 制定如何密集化数据的 对象
     * @param options 表示聚合管道的$densify管道阶段的可选字段
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/18 上午12:06
     */
    Children densify(final String field, final DensifyRange range, final DensifyOptions options);

    /* $densify阶段 end */

    /**
     * 如果缺少管道，请使用该方法构建
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午5:49
     */
    Children custom(final Bson bson);

}
