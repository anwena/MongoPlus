package com.anwen.mongo.mapper;

import com.anwen.mongo.aggregate.Aggregate;
import com.anwen.mongo.aware.MongoAwareUtils;
import com.anwen.mongo.aware.impl.NamespaceAware;
import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conditions.update.UpdateChainWrapper;
import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.mapping.TypeReference;
import com.anwen.mongo.model.MutablePair;
import com.anwen.mongo.model.PageResult;
import com.mongodb.client.model.CreateIndexOptions;
import com.mongodb.client.model.DropIndexOptions;
import com.mongodb.client.model.IndexModel;
import com.mongodb.client.model.IndexOptions;
import com.mongodb.client.model.WriteModel;
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

    public DefaultBaseMapperImpl(MongoPlusClient mongoPlusClient, MongoConverter mongoConverter) {
        super(mongoPlusClient, mongoConverter, new ExecutorFactory());
        this.mongoPlusClient = mongoPlusClient;
    }

    public DefaultBaseMapperImpl(MongoPlusClient mongoPlusClient, MongoConverter mongoConverter, ExecutorFactory factory) {
        super(mongoPlusClient, mongoConverter, factory);
        this.mongoPlusClient = mongoPlusClient;
    }

    @Override
    public <T> boolean save(T entity) {
        MutablePair<String, String> namespace = getNamespace(entity.getClass());
        return save(namespace.left, namespace.right, entity);
    }

    @Override
    public <T> Boolean saveBatch(Collection<T> entityList) {
        Class<?> clazz = entityList.iterator().next().getClass();
        MutablePair<String, String> namespace = getNamespace(clazz);
        return saveBatch(namespace.left, namespace.right, entityList);
    }

    @Override
    public Long update(Bson queryBasic, Bson updateBasic, Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return update(namespace.left, namespace.right, queryBasic, updateBasic);
    }

    @Override
    public Integer bulkWrite(List<WriteModel<Document>> writeModelList, Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return bulkWrite(namespace.left, namespace.right, writeModelList);
    }

    @Override
    public <T> Boolean update(T entity, QueryChainWrapper<T, ?> queryChainWrapper) {
        MutablePair<String, String> namespace = getNamespace(entity.getClass());
        return update(namespace.left, namespace.right, entity, queryChainWrapper);
    }

    /**
     * 查询所有
     *
     * @param clazz  操作的class
     * @param rClazz 返回的class
     * @return {@link List <T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    @Override
    public <T, R> List<R> list(Class<T> clazz, Class<R> rClazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return list(namespace.left, namespace.right, rClazz);
    }

    @Override
    public <T, R> List<R> list(Class<T> clazz, TypeReference<R> typeReference) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return list(namespace.left, namespace.right, typeReference);
    }

    @Override
    public <T, R> List<R> list(QueryChainWrapper<T, ?> queryChainWrapper, Class<T> clazz, Class<R> rClazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return list(namespace.left, namespace.right, queryChainWrapper, rClazz);
    }

    @Override
    public <T, R> List<R> list(QueryChainWrapper<T, ?> queryChainWrapper, Class<T> clazz, TypeReference<R> typeReference) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return list(namespace.left, namespace.right, queryChainWrapper, typeReference);
    }

    @Override
    public <T, R> List<R> aggregateList(AggregateChainWrapper<T, ?> queryChainWrapper, Class<T> clazz, Class<R> rClazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return aggregateList(namespace.left, namespace.right, queryChainWrapper, rClazz);
    }

    @Override
    public <T, R> List<R> aggregateList(AggregateChainWrapper<T, ?> queryChainWrapper, Class<T> clazz, TypeReference<R> typeReference) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return aggregateList(namespace.left, namespace.right, queryChainWrapper, typeReference);
    }

    @Override
    public <T, R> List<R> aggregateList(Aggregate<?> aggregate, Class<T> clazz, Class<R> rClazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return aggregateList(namespace.left, namespace.right, aggregate, rClazz);
    }

    @Override
    public <T, R> List<R> aggregateList(Aggregate<?> aggregate, Class<T> clazz, TypeReference<R> typeReference) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return aggregateList(namespace.left, namespace.right, aggregate, typeReference);
    }

    @Override
    public <T, R> R one(QueryChainWrapper<T, ?> queryChainWrapper, Class<T> clazz, Class<R> rClazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return one(namespace.left, namespace.right, queryChainWrapper, rClazz);
    }

    @Override
    public <T, R> R one(QueryChainWrapper<T, ?> queryChainWrapper, Class<T> clazz, TypeReference<R> typeReference) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return one(namespace.left, namespace.right, queryChainWrapper, typeReference);
    }

    @Override
    public <T, R> PageResult<R> page(QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize, Class<T> clazz, Class<R> rClazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return page(namespace.left, namespace.right, queryChainWrapper, pageNum, pageSize, rClazz);
    }

    @Override
    public <T, R> PageResult<R> page(QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize, Class<T> clazz, TypeReference<R> typeReference) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return page(namespace.left, namespace.right, queryChainWrapper, pageNum, pageSize, typeReference);
    }

    @Override
    public <T, R> List<R> pageList(QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize, Class<T> clazz, Class<R> rClazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return pageList(namespace.left, namespace.right, queryChainWrapper, pageNum, pageSize, rClazz);
    }

    @Override
    public <T, R> List<R> pageList(QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize, Class<T> clazz, TypeReference<R> typeReference) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return pageList(namespace.left, namespace.right, queryChainWrapper, pageNum, pageSize, typeReference);
    }

    @Override
    public <T, R> PageResult<R> page(QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize, Integer recentPageNum, Class<T> clazz, Class<R> rClazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return page(namespace.left, namespace.right, queryChainWrapper, pageNum, pageSize, recentPageNum, rClazz);
    }

    @Override
    public <T, R> PageResult<R> page(QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize, Integer recentPageNum, Class<T> clazz, TypeReference<R> typeReference) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return page(namespace.left, namespace.right, queryChainWrapper, pageNum, pageSize, recentPageNum, typeReference);
    }

    @Override
    public <T, R> R getById(Serializable id, Class<T> clazz, Class<R> rClazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return getById(namespace.left, namespace.right, id, rClazz);
    }

    @Override
    public <T, R> R getById(Serializable id, Class<T> clazz, TypeReference<R> typeReference) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return getById(namespace.left, namespace.right, id, typeReference);
    }

    @Override
    public boolean isExist(Serializable id, Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return isExist(namespace.left, namespace.right, id);
    }

    @Override
    public boolean isExist(QueryChainWrapper<?, ?> queryChainWrapper, Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return isExist(namespace.left, namespace.right, queryChainWrapper);
    }

    @Override
    public <T, R> List<R> getByIds(Collection<? extends Serializable> ids, Class<T> clazz, Class<R> rClazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return getByIds(namespace.left, namespace.right, ids, rClazz);
    }

    @Override
    public <T, R> List<R> getByIds(Collection<? extends Serializable> ids, Class<T> clazz, TypeReference<R> typeReference) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return getByIds(namespace.left, namespace.right, ids, typeReference);
    }

    @Override
    public Boolean update(UpdateChainWrapper<?, ?> updateChainWrapper, Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return update(namespace.left, namespace.right, updateChainWrapper);
    }

    @Override
    public Boolean remove(UpdateChainWrapper<?, ?> updateChainWrapper, Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return remove(namespace.left, namespace.right, updateChainWrapper);
    }

    @Override
    public Long remove(Bson filter, Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return remove(namespace.left, namespace.right, filter);
    }

    @Override
    public long count(QueryChainWrapper<?, ?> queryChainWrapper, Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return count(namespace.left, namespace.right, queryChainWrapper);
    }

    /**
     * 分页查询 查询总条数
     *
     * @param compareConditionList 条件集合
     * @param clazz                result class
     * @param pageNum              当前页
     * @param pageSize             每页显示行数
     * @param recentPageNum        查询最近n页的数据  {参数=null 表示仅查询当前页数据}  {参数取值[5-50] 表示查询最近[5-50]页的数据 建议recentPageNum等于10 参考 百度分页检索}
     * @return long
     */
    @Override
    public long recentPageCount(List<CompareCondition> compareConditionList, Class<?> clazz, Integer pageNum, Integer pageSize, Integer recentPageNum) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return recentPageCount(namespace.left, namespace.right, compareConditionList, pageNum, pageSize, recentPageNum);
    }

    @Override
    public long count(Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return count(namespace.left, namespace.right);
    }

    @Override
    public <T, R> List<R> queryCommand(String command, Class<T> clazz, Class<R> rClazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return queryCommand(namespace.left, namespace.right, command, rClazz);
    }

    @Override
    public <T, R> List<R> queryCommand(String command, Class<T> clazz, TypeReference<R> typeReference) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return queryCommand(namespace.left, namespace.right, command, typeReference);
    }

    @Override
    public <T, R> List<R> getByColumn(String column, Object value, Class<T> clazz, Class<R> rClazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return getByColumn(namespace.left, namespace.right, column, value, rClazz);
    }

    @Override
    public <T, R> List<R> getByColumn(String column, Object value, Class<T> clazz, TypeReference<R> typeReference) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return getByColumn(namespace.left, namespace.right, column, value, typeReference);
    }

    @Override
    public String createIndex(Bson bson, Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return createIndex(namespace.left, namespace.right, bson);
    }

    @Override
    public String createIndex(Bson bson, IndexOptions indexOptions, Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return createIndex(namespace.left, namespace.right, bson, indexOptions);
    }

    @Override
    public List<String> createIndexes(List<IndexModel> indexes, Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return createIndexes(namespace.left, namespace.right, indexes);
    }

    @Override
    public List<String> createIndexes(List<IndexModel> indexes, CreateIndexOptions createIndexOptions, Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return createIndexes(namespace.left, namespace.right, indexes, createIndexOptions);
    }

    @Override
    public List<Document> listIndexes(Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        return listIndexes(namespace.left, namespace.right);
    }

    @Override
    public void dropIndex(String indexName, Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        dropIndex(namespace.left, namespace.right, indexName);
    }

    @Override
    public void dropIndex(String indexName, DropIndexOptions dropIndexOptions, Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        dropIndex(namespace.left, namespace.right, indexName, dropIndexOptions);
    }

    @Override
    public void dropIndex(Bson keys, Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        dropIndex(namespace.left, namespace.right, keys);
    }

    @Override
    public void dropIndex(Bson keys, DropIndexOptions dropIndexOptions, Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        dropIndex(namespace.left, namespace.right, keys, dropIndexOptions);
    }

    @Override
    public void dropIndexes(Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        dropIndexes(namespace.left, namespace.right);
    }

    @Override
    public void dropIndexes(DropIndexOptions dropIndexOptions, Class<?> clazz) {
        MutablePair<String, String> namespace = getNamespace(clazz);
        dropIndexes(namespace.left, namespace.right, dropIndexOptions);
    }

    protected MutablePair<String, String> getNamespace(Class<?> clazz) {

        String database = mongoPlusClient.getDatabase(clazz);
        String collectionName = mongoPlusClient.getCollectionName(clazz);

        // 发布感知事件
        NamespaceAware.Namespace namespace = NamespaceAware.NamespaceBuild.builder()
                .dataBase(database).collectionName(collectionName).entityClass(clazz).build();
        MongoAwareUtils.doInvoke(NamespaceAware.class, namespace);

        return new MutablePair<>(database, collectionName);

    }

}
