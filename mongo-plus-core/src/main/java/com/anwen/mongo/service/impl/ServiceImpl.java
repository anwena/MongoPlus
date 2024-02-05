package com.anwen.mongo.service.impl;

import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.aggregate.LambdaAggregateChainWrapper;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.query.LambdaQueryChainWrapper;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conditions.query.QueryWrapper;
import com.anwen.mongo.conditions.update.LambdaUpdateChainWrapper;
import com.anwen.mongo.conditions.update.UpdateChainWrapper;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;
import com.anwen.mongo.service.IService;
import com.anwen.mongo.support.SFunction;
import com.anwen.mongo.toolkit.*;
import com.mongodb.BasicDBObject;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.*;
import javafx.util.Pair;
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

    private String database;

    public String getDatabase() {
        return database;
    }

    public void setDatabase(String database) {
        this.database = database;
    }

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
            Pair<BasicDBObject, BasicDBObject> updatePair = getUpdateCondition(queryChainWrapper.getCompareList(), entity);
            return baseMapper.update(updatePair.getKey(),updatePair.getValue(),ClassTypeUtil.getClass(entity)) >= 1;
        }
        return save(entity);
    }

    protected Pair<BasicDBObject,BasicDBObject> getUpdateCondition(List<CompareCondition> compareConditionList, T entity){
        BasicDBObject queryBasic = BuildCondition.buildQueryCondition(compareConditionList);
        Document document = DocumentUtil.checkUpdateField(entity,false);
        document.remove(SqlOperationConstant._ID);
        BasicDBObject updateField = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), document);
        return new Pair<>(queryBasic,updateField);
    }

    @Override
    public Boolean saveOrUpdateBatch(Collection<T> entityList) {
        List<WriteModel<Document>> writeModelList = new ArrayList<>();
        entityList.forEach(entity -> {
            String idByEntity = ClassTypeUtil.getIdByEntity(entity, true);
            if (StringUtils.isBlank(idByEntity)){
                writeModelList.add(new InsertOneModel<>(baseMapper.processIdField(entity,false)));
            } else {
                Pair<BasicDBObject,BasicDBObject> basicDBObjectPair = getUpdate(entity);
                writeModelList.add(new UpdateManyModel<>(basicDBObjectPair.getKey(),basicDBObjectPair.getValue()));
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
                Pair<BasicDBObject, BasicDBObject> updatePair = getUpdateCondition(queryChainWrapper.getCompareList(), entity);
                writeModelList.add(new UpdateManyModel<>(updatePair.getKey(),updatePair.getValue()));
            } else {
                writeModelList.add(new InsertOneModel<>(baseMapper.processIdField(entity,false)));
            }
        });
        return baseMapper.bulkWrite(writeModelList,entityList.stream().findFirst().get().getClass()) == entityList.size();
    }

    @Override
    public Boolean updateById(T entity) {
        Pair<BasicDBObject,BasicDBObject> basicDBObjectPair = getUpdate(entity);
        return baseMapper.update(basicDBObjectPair.getKey(),basicDBObjectPair.getValue(),ClassTypeUtil.getClass(entity)) >= 1;
    }

    protected <T> Pair<BasicDBObject,BasicDBObject> getUpdate(T entity) {
        Document document = DocumentUtil.checkUpdateField(entity,false);
        BasicDBObject filter = ExecuteUtil.getFilter(document);
        BasicDBObject update = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), document);
        return new Pair<>(filter,update);
    }

    @Override
    public Boolean updateBatchByIds(Collection<T> entityList) {
        List<WriteModel<Document>> writeModelList = new ArrayList<>();
        entityList.forEach(entity -> {
            Pair<BasicDBObject,BasicDBObject> basicDBObjectPair = getUpdate(entity);
            writeModelList.add(new UpdateManyModel<>(basicDBObjectPair.getKey(),basicDBObjectPair.getValue()));
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
        Bson filter = Filters.eq(column, ObjectId.isValid(valueOf) ? new ObjectId(valueOf) : filterValue);
        Document document = DocumentUtil.checkUpdateField(entity,false);
        return baseMapper.update(filter,document,ClassTypeUtil.getClass(entity)) >= 1;
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
        Bson filterId = Filters.eq(SqlOperationConstant._ID, ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id);
        return baseMapper.remove(filterId,clazz) >= 1;
    }

    @Override
    public Boolean removeByColumn(SFunction<T, Object> column, Object value) {
        return removeByColumn(column.getFieldNameLine(),value);
    }

    @Override
    public Boolean removeByColumn(String column, Object value) {
        Bson filter = Filters.eq(column, ObjectId.isValid(String.valueOf(value)) ? new ObjectId(String.valueOf(value)) : value);
        return baseMapper.remove(filter,clazz) >= 1;
    }

    @Override
    public Boolean removeBatchByIds(Collection<? extends Serializable> idList) {
        List<Serializable> convertedIds = idList.stream()
                .map(id -> ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id)
                .collect(Collectors.toList());
        Bson objectIdBson = Filters.in(SqlOperationConstant._ID, convertedIds);
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
    public T limitOne(QueryChainWrapper<T, ?> queryChainWrapper) {
        return baseMapper.limitOne(queryChainWrapper,clazz);
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
