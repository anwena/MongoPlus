package com.anwen.mongo.strategy.aggregate;

import com.mongodb.BasicDBObject;

/**
 * 管道策略接口
 * @author JiaChaoYang
 * @date 2023/8/19 0:05
*/
public interface PipelineStrategy {

    /**
     * 构建管道
     * @author JiaChaoYang
     * @date 2023/8/19 0:05
    */
    BasicDBObject buildAggregate();

}
