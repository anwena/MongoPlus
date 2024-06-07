package com.anwen.mongo.conditions.interfaces.aggregate.aggregate;

import com.anwen.mongo.model.aggregate.Field;
import com.anwen.mongo.support.SFunction;
import com.mongodb.client.model.BucketAutoOptions;
import com.mongodb.client.model.BucketOptions;
import org.bson.conversions.Bson;

import java.util.Collection;
import java.util.List;

public interface Aggregate<Children> {

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


    /**
     * 如果缺少管道，请使用该方法构建
     * @param bson bson
     * @return {@link Children}
     * @author anwen
     * @date 2024/6/7 下午5:49
     */
    Children custom(final Bson bson);

}
