package com.anwen.mongo.handlers;

import com.anwen.mongo.toolkit.BsonUtil;
import org.bson.conversions.Bson;

/**
 * 类型处理器，但只作用于实体类字段的赋值和转换
 * 类型处理器优先级比转换器和映射器高，因为TypeHandler作用于字段
 * 在实体类上使用类型处理器时，需要实现下边的两个方法，如果返回null，则继续走MongoPlus的处理，反之则直接使用类型处理器各个方法返回的数据
 * @author anwen
 * @date 2024/5/29 下午10:07
 */
public interface TypeHandler<T> {

    /**
     * 设置值。
     * 比如某个字段使用了BigInteger类型，但是MongoDB驱动是不支持该类型的，所以可以在这里将该值转换为Integer类型并返回（默认已在映射器支持）
     * @param obj 内容
     * @param bson 最终对象
     * @return {@link Object}
     * @author anwen
     * @date 2024/5/29 下午10:12
     */
    Object setParameter(String fieldName,T obj, Bson bson);

    /**
     * 获取结果
     * 可以将MongoDB中查询的值映射为指定类型，如字段使用InputStream，但是MongoDB中是Binary类型，那么就需要在这里进行转换，如：
     * return new ByteArrayInputStream(((Binary)obj).getData());
     * @param obj 数据库中的值
     * @return {@link T}
     * @author anwen
     * @date 2024/5/29 下午10:16
     */
    T getResult(Object obj);

}
