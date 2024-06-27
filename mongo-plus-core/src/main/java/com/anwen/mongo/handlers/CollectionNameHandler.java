package com.anwen.mongo.handlers;

import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.mongodb.MongoNamespace;

/**
 * 动态集合处理器
 * @author anwen
 * @date 2024/6/27 下午2:47
 * @since by mybatis-plus
 */
public interface CollectionNameHandler {

    /**
     * 生成动态表明
     * @param executeMethodEnum 当前的操作
     * @param source 当前操作的参数
     * @param namespace 命名空间
     * @return {@link java.lang.String}
     * @author anwen
     * @date 2024/6/27 下午2:53
     */
    String dynamicCollectionName(ExecuteMethodEnum executeMethodEnum,Object[] source, MongoNamespace namespace);

}
