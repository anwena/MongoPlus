package com.anwen.mongo.mapper;

import com.anwen.mongo.aggregate.Aggregate;
import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conditions.update.UpdateChainWrapper;
import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.mapping.TypeReference;
import com.anwen.mongo.model.PageResult;
import com.mongodb.client.model.*;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

/**
 * baseMapper默认实现
 *
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2024-02-05 11:47
 **/
public class DefaultBaseMapperImpl extends AbstractBaseMapper {

    private final MongoPlusClient mongoPlusClient;

    public DefaultBaseMapperImpl(MongoPlusClient mongoPlusClient,MongoConverter mongoConverter) {
        super(mongoPlusClient,mongoConverter,new ExecutorFactory());
        this.mongoPlusClient = mongoPlusClient;
    }

    public DefaultBaseMapperImpl(MongoPlusClient mongoPlusClient,MongoConverter mongoConverter,ExecutorFactory factory) {
        super(mongoPlusClient,mongoConverter,factory);
        this.mongoPlusClient = mongoPlusClient;
    }

    @Override
    public <T> boolean save(T entity){
        return save(getDateBase(entity.getClass()),getCollectionName(entity.getClass()),entity);
    }

    @Override
    public <T> Boolean saveBatch(Collection<T> entityList) {
        Class<?> clazz = entityList.iterator().next().getClass();
        return saveBatch(getDateBase(clazz),getCollectionName(clazz),entityList);
    }

    @Override
    public Long update(Bson queryBasic, Bson updateBasic, Class<?> clazz) {
        return update(getDateBase(clazz),getCollectionName(clazz),queryBasic,updateBasic);
    }

    @Override
    public Integer bulkWrite(List<WriteModel<Document>> writeModelList, Class<?> clazz) {
        return bulkWrite(getDateBase(clazz),getCollectionName(clazz),writeModelList);
    }

    @Override
    public <T> Boolean update(T entity,QueryChainWrapper<T,?> queryChainWrapper){
        return update(getDateBase(entity.getClass()),getCollectionName(entity.getClass()),entity,queryChainWrapper);
    }

    /**
     * 查询所有
     * @param clazz 操作的class
     * @param rClazz 返回的class
     * @return {@link List <T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    @Override
    public <T,R> List<R> list(Class<T> clazz,Class<R> rClazz) {
        return list(getDateBase(clazz),getCollectionName(clazz),rClazz);
    }

    @Override
    public <T, R> List<R> list(Class<T> clazz, TypeReference<R> typeReference) {
        return list(getDateBase(clazz),getCollectionName(clazz),typeReference);
    }

    @Override
    public <T,R> List<R> list(QueryChainWrapper<T,?> queryChainWrapper, Class<T> clazz,Class<R> rClazz) {
        return list(getDateBase(clazz),getCollectionName(clazz),queryChainWrapper,rClazz);
    }

    @Override
    public <T, R> List<R> list(QueryChainWrapper<T, ?> queryChainWrapper, Class<T> clazz, TypeReference<R> typeReference) {
        return list(getDateBase(clazz),getCollectionName(clazz),queryChainWrapper,typeReference);
    }

    @Override
    public <T,R> List<R> aggregateList(AggregateChainWrapper<T, ?> queryChainWrapper, Class<T> clazz,Class<R> rClazz){
        return aggregateList(getDateBase(clazz),getCollectionName(clazz),queryChainWrapper,rClazz);
    }

    @Override
    public <T, R> List<R> aggregateList(AggregateChainWrapper<T, ?> queryChainWrapper, Class<T> clazz, TypeReference<R> typeReference) {
        return aggregateList(getDateBase(clazz),getCollectionName(clazz),queryChainWrapper,typeReference);
    }

    @Override
    public <T, R> List<R> aggregateList(Aggregate<?> aggregate, Class<T> clazz, Class<R> rClazz) {
        return aggregateList(getDateBase(clazz),getCollectionName(clazz),aggregate,rClazz);
    }

    @Override
    public <T, R> List<R> aggregateList(Aggregate<?> aggregate, Class<T> clazz, TypeReference<R> typeReference) {
        return aggregateList(getDateBase(clazz),getCollectionName(clazz),aggregate,typeReference);
    }

    @Override
    public <T,R> R one(QueryChainWrapper<T,?> queryChainWrapper,Class<T> clazz,Class<R> rClazz) {
        return one(getDateBase(clazz),getCollectionName(clazz),queryChainWrapper,rClazz);
    }

    @Override
    public <T, R> R one(QueryChainWrapper<T, ?> queryChainWrapper, Class<T> clazz, TypeReference<R> typeReference) {
        return one(getDateBase(clazz),getCollectionName(clazz),queryChainWrapper,typeReference);
    }

    @Override
    public <T,R> PageResult<R> page(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize,Class<T> clazz,Class<R> rClazz) {
        return page(getDateBase(clazz),getCollectionName(clazz),queryChainWrapper,pageNum,pageSize,rClazz);
    }

    @Override
    public <T, R> PageResult<R> page(QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize, Class<T> clazz, TypeReference<R> typeReference) {
        return page(getDateBase(clazz),getCollectionName(clazz),queryChainWrapper,pageNum,pageSize,typeReference);
    }

    @Override
    public <T,R> List<R> pageList(QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize, Class<T> clazz,Class<R> rClazz) {
        return pageList(getDateBase(clazz),getCollectionName(clazz),queryChainWrapper,pageNum,pageSize,rClazz);
    }

    @Override
    public <T, R> List<R> pageList(QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize, Class<T> clazz, TypeReference<R> typeReference) {
        return pageList(getDateBase(clazz),getCollectionName(clazz),queryChainWrapper,pageNum,pageSize,typeReference);
    }

    @Override
    public <T,R> PageResult<R> page(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, Integer recentPageNum, Class<T> clazz,Class<R> rClazz) {
        return page(getDateBase(clazz),getCollectionName(clazz),queryChainWrapper,pageNum,pageSize,recentPageNum,rClazz);
    }

    @Override
    public <T, R> PageResult<R> page(QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize, Integer recentPageNum, Class<T> clazz, TypeReference<R> typeReference) {
        return page(getDateBase(clazz),getCollectionName(clazz),queryChainWrapper,pageNum,pageSize,recentPageNum,typeReference);
    }

    @Override
    public <T,R> R getById(Serializable id,Class<T> clazz,Class<R> rClazz) {
        return getById(getDateBase(clazz),getCollectionName(clazz),id,rClazz);
    }

    @Override
    public <T, R> R getById(Serializable id, Class<T> clazz, TypeReference<R> typeReference) {
        return getById(getDateBase(clazz),getCollectionName(clazz),id,typeReference);
    }

    @Override
    public boolean isExist(Serializable id,Class<?> clazz){
        return isExist(getDateBase(clazz),getCollectionName(clazz),id);
    }

    @Override
    public boolean isExist(QueryChainWrapper<?,?> queryChainWrapper,Class<?> clazz){
        return isExist(getDateBase(clazz),getCollectionName(clazz),queryChainWrapper);
    }

    @Override
    public <T,R> List<R> getByIds(Collection<? extends Serializable> ids,Class<T> clazz,Class<R> rClazz) {
        return getByIds(getDateBase(clazz),getCollectionName(clazz),ids,rClazz);
    }

    @Override
    public <T, R> List<R> getByIds(Collection<? extends Serializable> ids, Class<T> clazz, TypeReference<R> typeReference) {
        return getByIds(getDateBase(clazz),getCollectionName(clazz),ids,typeReference);
    }

    @Override
    public Boolean update(UpdateChainWrapper<?, ?> updateChainWrapper,Class<?> clazz) {
        return update(getDateBase(clazz),getCollectionName(clazz),updateChainWrapper);
    }

    @Override
    public Boolean remove(UpdateChainWrapper<?, ?> updateChainWrapper, Class<?> clazz) {
        return remove(getDateBase(clazz),getCollectionName(clazz),updateChainWrapper);
    }

    @Override
    public Long remove(Bson filter, Class<?> clazz) {
        return remove(getDateBase(clazz),getCollectionName(clazz),filter);
    }

    @Override
    public long count(QueryChainWrapper<?, ?> queryChainWrapper,Class<?> clazz){
        return count(getDateBase(clazz),getCollectionName(clazz),queryChainWrapper);
    }

    /**
     * 分页查询 查询总条数
     * @param compareConditionList 条件集合
     * @param clazz result class
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @param recentPageNum 查询最近n页的数据  {参数=null 表示仅查询当前页数据}  {参数取值[5-50] 表示查询最近[5-50]页的数据 建议recentPageNum等于10 参考 百度分页检索}
     * @return long
     */
    @Override
    public long recentPageCount(List<CompareCondition> compareConditionList,Class<?> clazz, Integer pageNum, Integer pageSize, Integer recentPageNum){
        return recentPageCount(getDateBase(clazz),getCollectionName(clazz),compareConditionList,pageNum,pageSize,recentPageNum);
    }

    @Override
    public long count(Class<?> clazz){
        return count(getDateBase(clazz),getCollectionName(clazz));
    }

    @Override
    public <T,R> List<R> queryCommand(String command,Class<T> clazz,Class<R> rClazz){
        return queryCommand(getDateBase(clazz),getCollectionName(clazz),command,rClazz);
    }

    @Override
    public <T, R> List<R> queryCommand(String command, Class<T> clazz, TypeReference<R> typeReference) {
        return queryCommand(getDateBase(clazz),getCollectionName(clazz),command,typeReference);
    }

    @Override
    public <T,R> List<R> getByColumn(String column,Object value,Class<T> clazz,Class<R> rClazz){
        return getByColumn(getDateBase(clazz),getCollectionName(clazz),column,value,rClazz);
    }

    @Override
    public <T, R> List<R> getByColumn(String column, Object value, Class<T> clazz, TypeReference<R> typeReference) {
        return getByColumn(getDateBase(clazz),getCollectionName(clazz),column,value,typeReference);
    }

    @Override
    public String createIndex(Bson bson,Class<?> clazz){
        return createIndex(getDateBase(clazz),getCollectionName(clazz),bson);
    }

    @Override
    public String createIndex(Bson bson, IndexOptions indexOptions, Class<?> clazz){
        return createIndex(getDateBase(clazz),getCollectionName(clazz),bson,indexOptions);
    }

    @Override
    public List<String> createIndexes(List<IndexModel> indexes,Class<?> clazz){
        return createIndexes(getDateBase(clazz),getCollectionName(clazz),indexes);
    }

    @Override
    public List<String> createIndexes(List<IndexModel> indexes, CreateIndexOptions createIndexOptions,Class<?> clazz){
        return createIndexes(getDateBase(clazz),getCollectionName(clazz),indexes,createIndexOptions);
    }

    @Override
    public List<Document> listIndexes(Class<?> clazz){
        return listIndexes(getDateBase(clazz),getCollectionName(clazz));
    }

    @Override
    public void dropIndex(String indexName,Class<?> clazz){
        dropIndex(getDateBase(clazz),getCollectionName(clazz),indexName);
    }

    @Override
    public void dropIndex(String indexName,DropIndexOptions dropIndexOptions,Class<?> clazz){
        dropIndex(getDateBase(clazz),getCollectionName(clazz),indexName,dropIndexOptions);
    }

    @Override
    public void dropIndex(Bson keys,Class<?> clazz){
        dropIndex(getDateBase(clazz),getCollectionName(clazz),keys);
    }

    @Override
    public void dropIndex(Bson keys,DropIndexOptions dropIndexOptions,Class<?> clazz){
        dropIndex(getDateBase(clazz),getCollectionName(clazz),keys,dropIndexOptions);
    }

    @Override
    public void dropIndexes(Class<?> clazz){
        dropIndexes(getDateBase(clazz),getCollectionName(clazz));
    }

    @Override
    public void dropIndexes(DropIndexOptions dropIndexOptions,Class<?> clazz){
        dropIndexes(getDateBase(clazz),getCollectionName(clazz),dropIndexOptions);
    }

    protected String getDateBase(Class<?> clazz){
        return mongoPlusClient.getDatabase(clazz);
    }

    protected String getCollectionName(Class<?> clazz){
        return mongoPlusClient.getCollectionName(clazz);
    }

}
