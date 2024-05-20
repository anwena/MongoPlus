package com.anwen.mongo.service.impl;

import com.anwen.mongo.annotation.collection.CollectionName;
import com.anwen.mongo.cache.global.DataSourceNameCache;
import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.aggregate.LambdaAggregateChainWrapper;
import com.anwen.mongo.conditions.query.LambdaQueryChainWrapper;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conditions.query.QueryWrapper;
import com.anwen.mongo.conditions.update.LambdaUpdateChainWrapper;
import com.anwen.mongo.conditions.update.UpdateChainWrapper;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.model.MutablePair;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;
import com.anwen.mongo.service.IService;
import com.anwen.mongo.support.SFunction;
import com.anwen.mongo.toolkit.ChainWrappers;
import com.anwen.mongo.toolkit.ClassTypeUtil;
import com.anwen.mongo.toolkit.ConditionUtil;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.BasicDBObject;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.*;
import org.bson.Document;
import org.bson.conversions.Bson;
import org.bson.types.ObjectId;

import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author JiaChaoYang
 * 接口实现
 * @since 2023-02-09 14:13
 **/
public class ServiceImpl<T> implements IService<T>{

    private BaseMapper baseMapper;

    public void setBaseMapper(BaseMapper baseMapper){
        this.baseMapper = baseMapper;
    }

    private Class<T> clazz;

    public void setClazz(Class<?> clazz) {
        this.clazz = (Class<T>) clazz;
    }

    @Override
    public Class<T> getGenericityClass() {
        if (clazz != null) {
            return clazz;
        }
        Type superClassType = getClass().getGenericSuperclass();
        ParameterizedType pt = (ParameterizedType) superClassType;
        Type genType = pt.getActualTypeArguments()[0];

        if (genType instanceof Class) {
            clazz = (Class<T>) genType;
        } else if (genType instanceof TypeVariable) {
            // 处理泛型类型是 TypeVariable 的情况
            clazz = (Class<T>) Object.class;
        } else {
            throw new IllegalArgumentException("Unsupported generic type: " + genType);
        }
        return clazz;
    }

    @Override
    public MongoCollection<Document> getCollection() {
        String database = DataSourceNameCache.getDatabase();
        CollectionName collectionName = clazz.getAnnotation(CollectionName.class);
        if (collectionName != null && StringUtils.isNotBlank(collectionName.database())){
            database = collectionName.database();
        }
        return baseMapper.getMongoPlusClient().getCollection(database,clazz);
    }

    @Override
    public MongoCollection<Document> getCollection(String database) {
        return baseMapper.getMongoPlusClient().getCollection(database,clazz);
    }

    @Override
    public Boolean save(T entity) {
        return baseMapper.save(entity);
    }

    @Override
    public Boolean saveBatch(Collection<T> entityList) {
        return baseMapper.saveBatch(entityList);
    }

    @Override
    public Boolean saveOrUpdate(T entity) {
        String idByEntity = ClassTypeUtil.getIdByEntity(entity, true);
        if (StringUtils.isBlank(idByEntity)){
            return save(entity);
        }
        return updateById(entity);
    }

    @Override
    public Boolean saveOrUpdateWrapper(T entity, QueryChainWrapper<T, ?> queryChainWrapper) {
        long count = count(queryChainWrapper);
        if (count > 0){
            return baseMapper.update(entity,queryChainWrapper);
        }
        return save(entity);
    }

    @Override
    public Boolean saveOrUpdateBatch(Collection<T> entityList) {
        List<WriteModel<Document>> writeModelList = new ArrayList<>();
        entityList.forEach(entity -> {
            String idByEntity = ClassTypeUtil.getIdByEntity(entity, true);
            if (StringUtils.isBlank(idByEntity)){
                writeModelList.add(new InsertOneModel<>(baseMapper.getMongoConverter().writeBySave(entity)));
            } else {
                MutablePair<BasicDBObject,BasicDBObject> basicDBObjectPair = ConditionUtil.getUpdate(entity,baseMapper.getMongoConverter());
                writeModelList.add(new UpdateManyModel<>(basicDBObjectPair.getLeft(),basicDBObjectPair.getRight()));
            }
        });
        return baseMapper.bulkWrite(writeModelList,entityList.stream().findFirst().get().getClass()) == entityList.size();
    }

    @Override
    public Boolean saveOrUpdateBatchWrapper(Collection<T> entityList, QueryChainWrapper<T, ?> queryChainWrapper) {
        Class<?> clazz = entityList.stream().findFirst().get().getClass();
        List<WriteModel<Document>> writeModelList = new ArrayList<>();
        entityList.forEach(entity -> {
            long count = baseMapper.count(queryChainWrapper, clazz);
            if (count > 0){
                MutablePair<BasicDBObject,BasicDBObject> updatePair = ConditionUtil.getUpdateCondition(queryChainWrapper.getCompareList(), entity,baseMapper.getMongoConverter());
                writeModelList.add(new UpdateManyModel<>(updatePair.getLeft(),updatePair.getRight()));
            } else {
                writeModelList.add(new InsertOneModel<>(baseMapper.getMongoConverter().writeBySave(entity)));
            }
        });
        return baseMapper.bulkWrite(writeModelList,entityList.stream().findFirst().get().getClass()) == entityList.size();
    }

    @Override
    public Boolean updateById(T entity) {
        MutablePair<BasicDBObject,BasicDBObject> basicDBObjectPair = ConditionUtil.getUpdate(entity,baseMapper.getMongoConverter());
        return baseMapper.update(basicDBObjectPair.getLeft(),basicDBObjectPair.getRight(),ClassTypeUtil.getClass(entity)) >= 1;
    }

    @Override
    public Boolean updateBatchByIds(Collection<T> entityList) {
        List<WriteModel<Document>> writeModelList = new ArrayList<>();
        entityList.forEach(entity -> {
            MutablePair<BasicDBObject,BasicDBObject> basicDBObjectPair = ConditionUtil.getUpdate(entity,baseMapper.getMongoConverter());
            writeModelList.add(new UpdateManyModel<>(basicDBObjectPair.getLeft(),basicDBObjectPair.getRight()));
        });
        return baseMapper.bulkWrite(writeModelList,entityList.stream().findFirst().get().getClass()) == entityList.size();
    }

    @Override
    public Boolean updateByColumn(T entity, SFunction<T, Object> column) {
        return updateByColumn(entity,column.getFieldNameLine());
    }

    @Override
    public Boolean updateByColumn(T entity, String column) {
        Object filterValue = ClassTypeUtil.getClassFieldValue(entity,column);
        String valueOf = String.valueOf(filterValue);
        BasicDBObject filter = new BasicDBObject(column, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(valueOf) ? new ObjectId(valueOf) : filterValue));
        Document document = baseMapper.getMongoConverter().writeByUpdate(entity);
        document.remove(column);
        BasicDBObject update = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), document);
        return baseMapper.update(filter,update,ClassTypeUtil.getClass(entity)) >= 1;
    }

    @Override
    public Boolean remove(UpdateChainWrapper<T, ?> updateChainWrapper) {
        return baseMapper.remove(updateChainWrapper,clazz);
    }

    @Override
    public Boolean update(UpdateChainWrapper<T, ?> updateChainWrapper) {
        return baseMapper.update(updateChainWrapper,clazz);
    }

    @Override
    public Boolean update(T entity, QueryChainWrapper<T, ?> queryChainWrapper) {
        return baseMapper.update(entity,queryChainWrapper);
    }

    @Override
    public Boolean removeById(Serializable id) {
        BasicDBObject filterId = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id));
        return baseMapper.remove(filterId,clazz) >= 1;
    }

    @Override
    public Boolean removeByColumn(SFunction<T, Object> column, Object value) {
        return removeByColumn(column.getFieldNameLine(),value);
    }

    @Override
    public Boolean removeByColumn(String column, Object value) {
        BasicDBObject filter = new BasicDBObject(column, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(value)) ? new ObjectId(String.valueOf(value)) : value));
        return baseMapper.remove(filter,clazz) >= 1;
    }

    @Override
    public Boolean removeBatchByIds(Collection<? extends Serializable> idList) {
        List<Serializable> convertedIds = idList.stream()
                .map(id -> ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id)
                .collect(Collectors.toList());
        BasicDBObject objectIdBson = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.IN.getCondition(), convertedIds));
        return baseMapper.remove(objectIdBson,clazz) >= 1;
    }

    @Override
    public List<T> aggregateList(AggregateChainWrapper<T, ?> queryChainWrapper) {
        return baseMapper.aggregateList(queryChainWrapper,clazz);
    }

    @Override
    public T one(QueryChainWrapper<T,?> queryChainWrapper) {
        return baseMapper.one(queryChainWrapper,clazz);
    }

    @Override
    public List<T> list() {
        return baseMapper.list(clazz);
    }

    @Override
    public List<T> list(QueryChainWrapper<T,?> queryChainWrapper) {
        return baseMapper.list(queryChainWrapper,clazz);
    }

    @Override
    public List<T> list(AggregateChainWrapper<T,?> queryChainWrapper) {
        return baseMapper.aggregateList(queryChainWrapper,clazz);
    }

    @Override
    public long count() {
        return baseMapper.count(clazz);
    }

    @Override
    public long count(QueryChainWrapper<T, ?> queryChainWrapper) {
        return baseMapper.count(queryChainWrapper,clazz);
    }

    @Override
    public PageResult<T> page(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize){
        return baseMapper.page(queryChainWrapper, pageNum,pageSize,clazz);
    }

    @Override
    public PageResult<T> page(QueryChainWrapper<T, ?> queryChainWrapper, PageParam pageParam) {
        return page(queryChainWrapper,pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<T> page(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, Integer recentPageNum){
        return baseMapper.page(queryChainWrapper, pageNum,pageSize,recentPageNum,clazz);
    }

    @Override
    public PageResult<T> page(QueryChainWrapper<T, ?> queryChainWrapper, PageParam pageParam, Integer recentPageNum) {
        return page(queryChainWrapper,pageParam.getPageNum(),pageParam.getPageSize(),recentPageNum);
    }

    @Override
    public List<T> pageList(PageParam pageParam) {
        return pageList(pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public List<T> pageList(Integer pageNum, Integer pageSize) {
        return baseMapper.pageList(new QueryWrapper<>(),pageNum,pageSize,clazz);
    }

    @Override
    public List<T> pageList(QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize) {
        return baseMapper.pageList(queryChainWrapper,pageNum,pageSize,clazz);
    }

    @Override
    public List<T> pageList(QueryChainWrapper<T, ?> queryChainWrapper, PageParam pageParam) {
        return baseMapper.pageList(queryChainWrapper,pageParam.getPageNum(),pageParam.getPageSize(),clazz);
    }

    @Override
    public PageResult<T> page(PageParam pageParam) {
        return page(pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<T> page(PageParam pageParam, Integer recentPageNum) {
        return page(pageParam.getPageNum(), pageParam.getPageSize(), recentPageNum);
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize) {
        return page(new QueryWrapper<>(),pageNum,pageSize);
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize, Integer recentPageNum) {
        return baseMapper.page(new QueryWrapper<>(),pageNum,pageSize,recentPageNum,clazz);
    }

    @Override
    public T getById(Serializable id) {
        return baseMapper.getById(id,clazz);
    }

    @Override
    public List<T> getByIds(Collection<? extends Serializable> ids) {
        return baseMapper.getByIds(ids,clazz);
    }

    @Override
    public List<T> queryCommand(String command) {
        return baseMapper.queryCommand(command,clazz);
    }

    @Override
    public List<T> getByColumn(SFunction<T, Object> field, Object fieldValue) {
        return baseMapper.getByColumn(field.getFieldNameLine(), fieldValue,clazz);
    }

    @Override
    public List<T> getByColumn(String field, Object fieldValue) {
        return baseMapper.getByColumn(field,fieldValue,clazz);
    }

    @Override
    public Boolean exist(Serializable id) {
        return baseMapper.isExist(id,clazz);
    }

    @Override
    public Boolean exist(QueryChainWrapper<T, ?> queryChainWrapper) {
        return baseMapper.isExist(queryChainWrapper,clazz);
    }

    @Override
    public String createIndex(Bson bson) {
        return baseMapper.createIndex(bson,clazz);
    }

    @Override
    public String createIndex(Bson bson, IndexOptions indexOptions) {
        return baseMapper.createIndex(bson,indexOptions,clazz);
    }

    @Override
    public List<String> createIndexes(List<IndexModel> indexes) {
        return baseMapper.createIndexes(indexes,clazz);
    }

    @Override
    public List<String> createIndexes(List<IndexModel> indexes, CreateIndexOptions createIndexOptions) {
        return baseMapper.createIndexes(indexes,createIndexOptions,clazz);
    }

    @Override
    public List<Document> listIndexes() {
        return baseMapper.listIndexes(clazz);
    }

    @Override
    public void dropIndex(String indexName) {
        baseMapper.dropIndex(indexName,clazz);
    }

    @Override
    public void dropIndex(String indexName, DropIndexOptions dropIndexOptions) {
        baseMapper.dropIndex(indexName,dropIndexOptions,clazz);
    }

    @Override
    public void dropIndex(Bson keys) {
        baseMapper.dropIndex(keys,clazz);
    }

    @Override
    public void dropIndex(Bson keys, DropIndexOptions dropIndexOptions) {
        baseMapper.dropIndex(keys,dropIndexOptions,clazz);
    }

    @Override
    public void dropIndexes() {
        baseMapper.dropIndexes(clazz);
    }

    @Override
    public void dropIndexes(DropIndexOptions dropIndexOptions) {
        baseMapper.dropIndexes(dropIndexOptions,clazz);
    }

    public Class<T> getClazz() {
        return clazz;
    }

    @Override
    public LambdaQueryChainWrapper<T> lambdaQuery() {
        return ChainWrappers.lambdaQueryChain(baseMapper,clazz);
    }

    @Override
    public LambdaAggregateChainWrapper<T> lambdaAggregate() {
        return ChainWrappers.lambdaAggregateChain(baseMapper,clazz);
    }

    @Override
    public LambdaUpdateChainWrapper<T> lambdaUpdate() {
        return ChainWrappers.lambdaUpdateChain(baseMapper,clazz);
    }
}
