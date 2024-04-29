package com.anwen.mongo.logic;

import com.anwen.mongo.cache.global.ClassLogicDeleteCache;
import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.enums.AggregateTypeEnum;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.model.AggregateBasicDBObject;
import com.anwen.mongo.model.BaseAggregate;
import com.anwen.mongo.model.MutablePair;
import com.anwen.mongo.model.QueryParam;
import com.anwen.mongo.strategy.aggregate.impl.DefaultConcretePipeline;
import com.mongodb.BasicDBObject;
import com.mongodb.client.model.CountOptions;
import com.mongodb.client.model.UpdateManyModel;
import com.mongodb.client.model.WriteModel;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class CollectionLogiceInterceptor implements Interceptor {

    @Override
    public MutablePair<Bson, Bson> executeUpdate(Bson queryBasic, Bson updateBasic) {

        Class<?> clazz = ClassLogicDeleteCache.getLogicCollection();
        if (LogicDeleteHandler.close() || Objects.isNull(clazz)) {
            return Interceptor.super.executeUpdate(queryBasic, updateBasic);
        }
        Bson query = LogicDeleteHandler.doBsonLogicDel(queryBasic, clazz);
        return new MutablePair<>(query, updateBasic);

    }

    @Override
    public QueryParam executeQuery(Bson queryBasic, BasicDBObject projectionList, BasicDBObject sortCond) {

        Class<?> clazz = ClassLogicDeleteCache.getLogicCollection();
        if (LogicDeleteHandler.close() || Objects.isNull(clazz)) {
            return Interceptor.super.executeQuery(queryBasic, projectionList, sortCond);
        }
        BasicDBObject query = (BasicDBObject) LogicDeleteHandler.doBsonLogicDel(queryBasic, clazz);
        return new QueryParam(query, projectionList, sortCond);

    }

    @Override
    public List<AggregateBasicDBObject> executeAggregate(List<AggregateBasicDBObject> aggregateConditionList) {

        Class<?> clazz = ClassLogicDeleteCache.getLogicCollection();
        if (LogicDeleteHandler.close() || Objects.isNull(clazz)) {
            return Interceptor.super.executeAggregate(aggregateConditionList);
        }
        BasicDBObject logicDelete = BuildCondition.buildQueryCondition(LogicDeleteHandler.doWrapperLogicDel(clazz));
        BaseAggregate aggregate = new BaseAggregate(AggregateTypeEnum.MATCH.getType(), new DefaultConcretePipeline(logicDelete), aggregateConditionList.size() + 1);
        aggregateConditionList.add(new AggregateBasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate(), aggregate.getOrder()));
        return aggregateConditionList;

    }

    @Override
    public MutablePair<BasicDBObject, CountOptions> executeCount(BasicDBObject queryBasic, CountOptions countOptions) {

        Class<?> clazz = ClassLogicDeleteCache.getLogicCollection();
        if (LogicDeleteHandler.close() || Objects.isNull(clazz)) {
            return Interceptor.super.executeCount(queryBasic, countOptions);
        }
        BasicDBObject query = (BasicDBObject) LogicDeleteHandler.doBsonLogicDel(queryBasic, clazz);
        return new MutablePair<>(query, countOptions);

    }

    @Override
    public List<WriteModel<Document>> executeBulkWrite(List<WriteModel<Document>> writeModelList) {

        Class<?> clazz = ClassLogicDeleteCache.getLogicCollection();
        if (LogicDeleteHandler.close() || Objects.isNull(clazz)) {
            return Interceptor.super.executeBulkWrite(writeModelList);
        }
        return writeModelList.stream().map(item -> {
            if (item instanceof UpdateManyModel) {
                UpdateManyModel umm = (UpdateManyModel) item;
                Bson filter = LogicDeleteHandler.doBsonLogicDel(umm.getFilter(), clazz);
                return new UpdateManyModel<Document>(filter, umm.getUpdate());
            }
            return item;
        }).collect(Collectors.toList());

    }

}
