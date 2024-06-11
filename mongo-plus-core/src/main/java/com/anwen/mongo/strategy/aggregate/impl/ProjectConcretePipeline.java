package com.anwen.mongo.strategy.aggregate.impl;

import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.project.Projection;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.enums.ProjectionEnum;
import com.anwen.mongo.strategy.aggregate.PipelineStrategy;
import com.mongodb.BasicDBObject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * project策略实现类
 *
 * @author JiaChaoYang
 **/
public class ProjectConcretePipeline implements PipelineStrategy {

    private final List<Projection> projectionList;

    public ProjectConcretePipeline(Projection... projection) {
        this.projectionList = new ArrayList<>(Arrays.asList(projection));
    }

    public ProjectConcretePipeline(boolean displayId, Projection... projection) {
        this.projectionList = new ArrayList<>(Arrays.asList(projection));
        if (!displayId) {
            this.projectionList.add(Projection.builder().column(SqlOperationConstant._ID).value(ProjectionEnum.NONE.getValue()).build());
        }
    }

    public ProjectConcretePipeline(List<Projection> projectionList) {
        this.projectionList = projectionList;
    }

    @Override
    public BasicDBObject buildAggregate() {
        return BuildCondition.buildProjection(projectionList);
    }
}
