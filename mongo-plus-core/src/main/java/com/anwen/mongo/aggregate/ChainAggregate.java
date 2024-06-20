package com.anwen.mongo.aggregate;

import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;

import java.util.List;

public interface ChainAggregate<T> {

    /**
     * 获取列表 返回T类型的List
     * @return {@link List<T>}
     * @author JiaChaoYang
     * @date 2023/7/20 23:13
     */
    List<T> list();

    /**
     * 获取列表，返回Class<R>类型的List
     * @param rClazz 返回类型
     * @return {@link List<R>}
     * @author anwen
     * @date 2024/6/19 下午11:37
     */
    <R> List<R> list(Class<R> rClazz);

}
