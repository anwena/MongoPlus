package com.anwen.mongo.execute;

import com.anwen.mongo.annotation.collection.CollectionName;
import com.anwen.mongo.cache.global.MongoClientCache;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.conn.ConnectMongoDB;
import com.anwen.mongo.context.MongoTransactionContext;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.domain.InitMongoCollectionException;
import com.anwen.mongo.execute.instance.DefaultExecute;
import com.anwen.mongo.execute.instance.SessionExecute;
import com.anwen.mongo.model.BaseProperty;
import com.anwen.mongo.model.SlaveDataSource;
import com.anwen.mongo.toolkit.CollUtil;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.MongoException;
import com.mongodb.client.ClientSession;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import org.bson.Document;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 执行器工厂
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2023-12-28 10:55
 **/
public class ExecutorFactory {

    private final Logger logger = LoggerFactory.getLogger(ExecutorFactory.class);

    /**
     * mongo客户端
     * @author JiaChaoYang
     * @date 2023/12/28 10:59
     */
//    private MongoClient mongoClient;

    /**
     * 从数据源
     * @author JiaChaoYang
     * @date 2023/12/28 10:58
     */
    private List<SlaveDataSource> slaveDataSourceList;

    /**
     * 属性配置
     * @author JiaChaoYang
     * @date 2023/12/28 10:59
     */
    private BaseProperty baseProperty;

    /**
     * 集合名策略
     * @author JiaChaoYang
     * @date 2023/12/28 11:44
    */
    private CollectionNameConvert collectionNameConvert;

    /**
     * 连接管理器
     * @author JiaChaoYang
     * @date 2023/12/28 14:15
    */
    private final Map<String,CollectionManager> collectionManagerMap = new ConcurrentHashMap<>();

    /**
     * 初始化工厂
     * @author JiaChaoYang
     * @date 2023/12/28 14:50
    */
    public void init(Class<?> clazz) {
        String collectionName = clazz.getSimpleName().toLowerCase();
        String dataSourceName = "master";
        String database = baseProperty.getDatabase();
        if (clazz.isAnnotationPresent(CollectionName.class)) {
            CollectionName annotation = clazz.getAnnotation(CollectionName.class);
            collectionName = annotation.value();
            String dataSource = annotation.dataSource();
            if (StringUtils.isNotBlank(dataSource) && CollUtil.isNotEmpty(slaveDataSourceList)) {
                Optional<SlaveDataSource> matchingSlave = slaveDataSourceList.stream()
                        .filter(slave -> Objects.equals(dataSource, slave.getSlaveName()))
                        .findFirst();
                if (matchingSlave.isPresent()) {
                    SlaveDataSource slave = matchingSlave.get();
                    database = slave.getDatabase();
                    dataSourceName = slave.getSlaveName();
                } else {
                    throw new InitMongoCollectionException("No matching slave data source configured");
                }
            }
        }
        try {
            MongoClient mongoClient = getMongoClient(dataSourceName);
            ConnectMongoDB connectMongoDB = new ConnectMongoDB(mongoClient, database, collectionName);
            MongoCollection<Document> collection = connectMongoDB.open();
            CollectionManager collectionManager;
            if (collectionManagerMap.containsKey(dataSourceName)){
                collectionManager = collectionManagerMap.get(dataSourceName);
                collectionManager.setCollectionMap(collectionName,collection);
            }else {
                collectionManager = new CollectionManager(mongoClient, collectionNameConvert, database);
                collectionManager.setCollectionMap(collectionName,collection);
            }
            collectionManagerMap.put(dataSourceName,collectionManager);
        } catch (MongoException e) {
            logger.error("Failed to connect to MongoDB: {}", e.getMessage(), e);
        }
    }

    private MongoClient getMongoClient(String dataSourceName){
        return MongoClientCache.mongoClientMap.get(dataSourceName);
    }

    /**
     * 获取执行器
     * @author JiaChaoYang
     * @date 2023/12/28 14:49
    */
    public AbstractExecute getExecute(String dataSourceName) {
        ClientSession clientSessionContext = MongoTransactionContext.getClientSessionContext();
        CollectionManager collectionManager = collectionManagerMap.get(dataSourceName);
        if (clientSessionContext != null) {
            return new SessionExecute(collectionNameConvert,collectionManager,clientSessionContext);
        }
        return new DefaultExecute(collectionNameConvert,collectionManager);
    }

//    /**
//     * 获取自定义执行器
//     * @param clazz 执行器class
//     * @param expandParam 拓展参数
//     * @return com.anwen.mongo.execute.Execute
//     * @author JiaChaoYang
//     * @date 2023/12/28 16:03
//    */
//    public AbstractExecute getExecute(Class<? extends AbstractExecute> clazz, Map<Object,Object> expandParam){
//        Constructor<?> constructor;
//        try {
//            constructor = clazz.getConstructor(CollectionNameConvert.class,CollectionManager.class,Map.class);
//        } catch (NoSuchMethodException e) {
//            logger.error("In the class of an extended executor, there must be a constructor with the following parameters: ‘MongoClient MongoClient, BaseProperty BaseProperty, CollectionNameConvert CollectionNameConvert, CollectionManager CollectionManager, Map<Object, Object>expandExecuteMap’");
//            throw new RuntimeException(e);
//        }
//        try {
//            return (AbstractExecute) constructor.newInstance(collectionNameConvert,collectionManagerMap.get(),expandParam);
//        } catch (InstantiationException | IllegalAccessException | InvocationTargetException e) {
//            logger.error("Instance creation failed, exception reason: {}",e.getMessage());
//            throw new RuntimeException(e);
//        }
//    }

    public static ExecuteFactoryBuilder builder() {
        return new ExecuteFactoryBuilder();
    }

    public List<SlaveDataSource> getSlaveDataSourceList() {
        return this.slaveDataSourceList;
    }

    public BaseProperty getBaseProperty() {
        return this.baseProperty;
    }

    public CollectionNameConvert getCollectionNameConvert() {
        return this.collectionNameConvert;
    }

    public void setSlaveDataSourceList(List<SlaveDataSource> slaveDataSourceList) {
        this.slaveDataSourceList = slaveDataSourceList;
    }

    public void setBaseProperty(BaseProperty baseProperty) {
        this.baseProperty = baseProperty;
    }

    public void setCollectionNameConvert(CollectionNameConvert collectionNameConvert) {
        this.collectionNameConvert = collectionNameConvert;
    }

    protected boolean canEqual(Object other) {
        return other instanceof ExecutorFactory;
    }

    @Override
    public String toString() {
        return "ExecutorFactory{" +
                "slaveDataSourceList=" + slaveDataSourceList +
                ", baseProperty=" + baseProperty +
                ", collectionNameConvert=" + collectionNameConvert +
                ", collectionManagerMap=" + collectionManagerMap +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ExecutorFactory)) return false;
        ExecutorFactory that = (ExecutorFactory) o;
        return Objects.equals(logger, that.logger) && Objects.equals(getSlaveDataSourceList(), that.getSlaveDataSourceList()) && Objects.equals(getBaseProperty(), that.getBaseProperty()) && Objects.equals(getCollectionNameConvert(), that.getCollectionNameConvert()) && Objects.equals(collectionManagerMap, that.collectionManagerMap);
    }

    @Override
    public int hashCode() {
        return Objects.hash(logger, getSlaveDataSourceList(), getBaseProperty(), getCollectionNameConvert(), collectionManagerMap);
    }

    public ExecutorFactory(List<SlaveDataSource> slaveDataSourceList, BaseProperty baseProperty, CollectionNameConvert collectionNameConvert) {
        this.slaveDataSourceList = slaveDataSourceList;
        this.baseProperty = baseProperty;
        this.collectionNameConvert = collectionNameConvert;
    }

    public ExecutorFactory() {
    }

    public static class ExecuteFactoryBuilder {
        private Map<String, MongoCollection<Document>> collectionMap;
        private String dateSourceName;
        private List<SlaveDataSource> slaveDataSourceList;
        private BaseProperty baseProperty;
        private ConnectMongoDB connectMongoDB;
        private CollectionNameConvert collectionNameConvert;

        ExecuteFactoryBuilder() {
        }

        public ExecuteFactoryBuilder collectionMap(Map<String, MongoCollection<Document>> collectionMap) {
            this.collectionMap = collectionMap;
            return this;
        }

        public ExecuteFactoryBuilder dataSourceName(String dateSourceName) {
            this.dateSourceName = dateSourceName;
            return this;
        }

        public ExecuteFactoryBuilder slaveDataSourceList(List<SlaveDataSource> slaveDataSourceList) {
            this.slaveDataSourceList = slaveDataSourceList;
            return this;
        }

        public ExecuteFactoryBuilder baseProperty(BaseProperty baseProperty) {
            this.baseProperty = baseProperty;
            return this;
        }

        public ExecuteFactoryBuilder connectMongoDB(ConnectMongoDB connectMongoDB) {
            this.connectMongoDB = connectMongoDB;
            return this;
        }

        public ExecuteFactoryBuilder collectionNameConvert(CollectionNameConvert collectionNameConvert) {
            this.collectionNameConvert = collectionNameConvert;
            return this;
        }

        public ExecutorFactory build() {
            return new ExecutorFactory(this.slaveDataSourceList, this.baseProperty, this.collectionNameConvert);
        }

        public String toString() {
            return "ExecuteFactory.ExecuteFactoryBuilder(collectionMap=" + this.collectionMap + ", dateSourceName=" + this.dateSourceName + ", slaveDataSourceList=" + this.slaveDataSourceList + ", baseProperty=" + this.baseProperty + ", connectMongoDB=" + this.connectMongoDB + ", collectionNameConvert=" + this.collectionNameConvert + ")";
        }
    }

}
