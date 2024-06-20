package com.anwen.mongo.aggregate.pipeline;

import com.anwen.mongo.aggregate.Aggregate;
import org.bson.conversions.Bson;

import java.util.List;

/**
 * 用于 $facet 管道阶段的类，封装Facet，支持lambda
 * @author anwen
 * @date 2024/6/11 下午8:05
 */
public class Facet extends com.mongodb.client.model.Facet {
    
    /**
     * $facet阶段
     * @param name facet名称
     * @param pipeline facet管道
     * @author anwen
     * @date 2024/6/11 下午8:10
     */
    public Facet(String name, List<? extends Bson> pipeline) {
        super(name, pipeline);
    }

    /**
     * $facet阶段
     * @param name facet名称
     * @param pipeline facet管道
     * @author anwen
     * @date 2024/6/11 下午8:10
     */
    public Facet(String name, Bson... pipeline) {
        super(name, pipeline);
    }

    /**
     * $facet阶段
     * @param name facet名称
     * @param aggregateChainWrapper 管道Wrapper
     * @author anwen
     * @date 2024/6/11 下午8:10
     */
    public Facet(String name, Aggregate<?> aggregateChainWrapper){
        super(name, aggregateChainWrapper.getAggregateConditionList());
    }
    
}
