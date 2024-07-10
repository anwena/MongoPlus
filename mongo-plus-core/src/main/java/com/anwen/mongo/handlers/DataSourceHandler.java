package com.anwen.mongo.handlers;

/**
 * 动态数据源处理器
 * @author anwen
 * @date 2024/7/9 下午5:14
 */
public interface DataSourceHandler {

    /**
     * 获取数据源
     * @param dsName {@link com.anwen.mongo.annotation.datasource.MongoDs}注解的value的值
     * @author anwen
     * @date 2024/7/9 下午5:15
     */
    String getDataSource(String dsName);

}
