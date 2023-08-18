package com.anwen.mongo.model;

import com.anwen.mongo.strategy.aggregate.PipelineStrategy;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author JiaChaoYang
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class BaseAggregate {

    private String type;

    private PipelineStrategy pipelineStrategy;

}
