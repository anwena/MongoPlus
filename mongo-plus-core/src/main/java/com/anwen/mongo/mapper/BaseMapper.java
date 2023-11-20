package com.anwen.mongo.mapper;

import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.aggregate.LambdaAggregateChainWrapper;
import com.anwen.mongo.conditions.query.LambdaQueryChainWrapper;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conditions.update.LambdaUpdateChainWrapper;
import com.anwen.mongo.conditions.update.UpdateChainWrapper;
import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;
import com.anwen.mongo.support.SFunction;
import com.mongodb.client.ClientSession;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

/**
 * @Description: 基本的Mapper接口
 * @Name: BaseMapper
 * @Author: Bomber
 * @CreateTime: 2023/11/16 15:01
 */
public interface BaseMapper<T> {

    /**
     * 根据id查询单个
     * @param id id
     * @return T
     * @author JiaChaoYang
     * @date 2023/6/25/025
     */
    T getById(Serializable id);
}
