package com.anwen.mongo.cache.global;

import com.anwen.mongo.constant.DataSourceConstant;
import com.anwen.mongo.toolkit.StringUtils;

/**
 * MongoClient缓存
 *
 * @author JiaChaoYang
 **/
public class DataSourceNameCache {

    /**
     * 只存入当前最新的数据源名称即可
     * 每次进行操作，都会经过{@link com.anwen.mongo.execute.ExecutorFactory#getExecute()}，通过这里存入的最新数据源名称去获取不同的ClientSession
     * @author JiaChaoYang
     * @date 2024/4/5 0:06
    */
    private static final ThreadLocal<String> dataSource = new InheritableThreadLocal<>();

    public static void setDataSource(String ds){
        dataSource.set(ds);
    }

    public static String getDataSource(){
        String ds = dataSource.get();
        if (StringUtils.isBlank(ds)){
            ds = DataSourceConstant.DEFAULT_DATASOURCE;
        }
        return ds;
    }

    public static void clear(){
        dataSource.remove();
    }

}
