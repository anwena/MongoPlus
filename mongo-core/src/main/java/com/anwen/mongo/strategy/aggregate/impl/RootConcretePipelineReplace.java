package com.anwen.mongo.strategy.aggregate.impl;

import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.ReplaceRoot;
import com.anwen.mongo.strategy.aggregate.PipelineStrategy;
import com.anwen.mongo.support.SFunction;
import com.mongodb.BasicDBObject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * replaceRoot策略实现类
 *
 * @author JiaChaoYang
 **/
public class RootConcretePipelineReplace implements PipelineStrategy {

    private final Boolean reserveOriginalDocument;

    private final List<ReplaceRoot> replaceRootList;

    public RootConcretePipelineReplace(Boolean reserveOriginalDocument, ReplaceRoot... replaceRoot) {
        this.reserveOriginalDocument = reserveOriginalDocument;
        this.replaceRootList = new ArrayList<>(Arrays.asList(replaceRoot));
    }

    public RootConcretePipelineReplace(Boolean reserveOriginalDocument, String... field) {
        this.reserveOriginalDocument = reserveOriginalDocument;
        this.replaceRootList = new ArrayList<ReplaceRoot>(){{
            for (String col : field) {
                add(new ReplaceRoot(col,col));
            }
        }};
    }

    @SafeVarargs
    public <T> RootConcretePipelineReplace(Boolean reserveOriginalDocument, SFunction<T,Object>... field) {
        this.reserveOriginalDocument = reserveOriginalDocument;
        this.replaceRootList = new ArrayList<ReplaceRoot>(){{
            for (SFunction<T,Object> col : field) {
                add(new ReplaceRoot(col.getFieldNameLine(),col.getFieldNameLine()));
            }
        }};
    }

    public RootConcretePipelineReplace(Boolean reserveOriginalDocument, List<ReplaceRoot> replaceRootList) {
        this.reserveOriginalDocument = reserveOriginalDocument;
        this.replaceRootList = replaceRootList;
    }

    @Override
    public BasicDBObject buildAggregate() {
        return BuildCondition.buildReplaceRoot(reserveOriginalDocument,replaceRootList);
    }
}
