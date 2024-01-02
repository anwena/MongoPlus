package com.anwen.mongo.execute;

import com.anwen.mongo.annotation.collection.CollectionName;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.conn.ConnectMongoDB;
import com.anwen.mongo.context.MongoTransactionContext;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.domain.InitMongoCollectionException;
import com.anwen.mongo.execute.instance.DefaultExecute;
import com.anwen.mongo.execute.instance.SessionExecute;
import com.anwen.mongo.model.BaseProperty;
import com.anwen.mongo.model.SlaveDataSource;
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
    private MongoClient mongoClient;

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
     * 实例化 ConnectMongoDB 对象，用于保存连接
     * @author JiaChaoYang
     * @date 2023/12/28 10:59
     */
    private ConnectMongoDB connectMongoDB;

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
    private CollectionManager collectionManager;

    /**
     * 初始化工厂
     * @author JiaChaoYang
     * @date 2023/12/28 14:50
    */
    public void init(Class<?> clazz) {
        String tableName = clazz.getSimpleName().toLowerCase();
        if (clazz.isAnnotationPresent(CollectionName.class)) {
            CollectionName annotation = clazz.getAnnotation(CollectionName.class);
            tableName = annotation.value();
            String dataSource = annotation.dataSource();
            if (StringUtils.isNotBlank(dataSource)) {
                Optional<SlaveDataSource> matchingSlave = slaveDataSourceList.stream()
                        .filter(slave -> Objects.equals(dataSource, slave.getSlaveName()))
                        .findFirst();
                if (matchingSlave.isPresent()) {
                    SlaveDataSource slave = matchingSlave.get();
                    baseProperty.setHost(slave.getHost());
                    baseProperty.setPort(slave.getPort());
                    baseProperty.setDatabase(slave.getDatabase());
                    baseProperty.setUsername(slave.getUsername());
                    baseProperty.setPassword(slave.getPassword());
                } else {
                    throw new InitMongoCollectionException("No matching slave data source configured");
                }
            }
        }
        try {
            connectMongoDB = new ConnectMongoDB(mongoClient, baseProperty.getDatabase(), tableName);
            MongoCollection<Document> collection = connectMongoDB.open();
            collectionManager = new CollectionManager(mongoClient,collectionNameConvert,baseProperty);
            collectionManager.setCollectionMap(tableName, collection);
        } catch (MongoException e) {
            logger.error("Failed to connect to MongoDB: {}", e.getMessage(), e);
        }
    }

    /**
     * 获取执行器
     * @author JiaChaoYang
     * @date 2023/12/28 14:49
    */
    public AbstractExecute getExecute(){
        ClientSession clientSessionContext = MongoTransactionContext.getClientSessionContext();
        if (clientSessionContext != null) {
            return new SessionExecute(mongoClient,baseProperty, collectionNameConvert,collectionManager,clientSessionContext);
        }
        return new DefaultExecute(mongoClient,baseProperty, collectionNameConvert,collectionManager);
    }

    /**
     * 获取自定义执行器
     * @param clazz 执行器class
     * @param expandParam 拓展参数
     * @return com.anwen.mongo.execute.Execute
     * @author JiaChaoYang
     * @date 2023/12/28 16:03
    */
    public AbstractExecute getExecute(Class<? extends AbstractExecute> clazz, Map<Object,Object> expandParam){
        Constructor<?> constructor;
        try {
            constructor = clazz.getConstructor(MongoClient.class, BaseProperty.class, CollectionNameConvert.class,CollectionManager.class,Map.class);
        } catch (NoSuchMethodException e) {
            logger.error("In the class of an extended executor, there must be a constructor with the following parameters: ‘MongoClient MongoClient, BaseProperty BaseProperty, CollectionNameConvert CollectionNameConvert, CollectionManager CollectionManager, Map<Object, Object>expandExecuteMap’");
            throw new RuntimeException(e);
        }
        try {
            return (AbstractExecute) constructor.newInstance(mongoClient,baseProperty,collectionNameConvert,collectionManager,expandParam);
        } catch (InstantiationException | IllegalAccessException | InvocationTargetException e) {
            logger.error("Instance creation failed, exception reason: {}",e.getMessage());
            throw new RuntimeException(e);
        }
    }

    public static ExecuteFactoryBuilder builder() {
        return new ExecuteFactoryBuilder();
    }

    public MongoClient getMongoClient() {
        return this.mongoClient;
    }

    public List<SlaveDataSource> getSlaveDataSourceList() {
        return this.slaveDataSourceList;
    }

    public BaseProperty getBaseProperty() {
        return this.baseProperty;
    }

    public ConnectMongoDB getConnectMongoDB() {
        return this.connectMongoDB;
    }

    public CollectionNameConvert getCollectionNameConvert() {
        return this.collectionNameConvert;
    }

    public void setMongoClient(MongoClient mongoClient) {
        this.mongoClient = mongoClient;
    }

    public void setSlaveDataSourceList(List<SlaveDataSource> slaveDataSourceList) {
        this.slaveDataSourceList = slaveDataSourceList;
    }

    public void setBaseProperty(BaseProperty baseProperty) {
        this.baseProperty = baseProperty;
    }

    public void setConnectMongoDB(ConnectMongoDB connectMongoDB) {
        this.connectMongoDB = connectMongoDB;
    }

    public void setCollectionNameConvert(CollectionNameConvert collectionNameConvert) {
        this.collectionNameConvert = collectionNameConvert;
    }

    protected boolean canEqual(Object other) {
        return other instanceof ExecutorFactory;
    }

    public int hashCode() {
        int result = 1;
        result = result * 59 + (logger == null ? 43 : logger.hashCode());
        Object $mongoClient = this.getMongoClient();
        result = result * 59 + ($mongoClient == null ? 43 : $mongoClient.hashCode());
        Object $slaveDataSourceList = this.getSlaveDataSourceList();
        result = result * 59 + ($slaveDataSourceList == null ? 43 : $slaveDataSourceList.hashCode());
        Object $baseProperty = this.getBaseProperty();
        result = result * 59 + ($baseProperty == null ? 43 : $baseProperty.hashCode());
        Object $connectMongoDB = this.getConnectMongoDB();
        result = result * 59 + ($connectMongoDB == null ? 43 : $connectMongoDB.hashCode());
        Object $collectionNameConvert = this.getCollectionNameConvert();
        result = result * 59 + ($collectionNameConvert == null ? 43 : $collectionNameConvert.hashCode());
        return result;
    }

    public String toString() {
        return "ExecuteFactory(logger=" + logger + ", mongoClient=" + this.getMongoClient() + ", slaveDataSourceList=" + this.getSlaveDataSourceList() + ", baseProperty=" + this.getBaseProperty() + ", connectMongoDB=" + this.getConnectMongoDB() + ", collectionNameConvert=" + this.getCollectionNameConvert() + ")";
    }

    public ExecutorFactory(MongoClient mongoClient, List<SlaveDataSource> slaveDataSourceList, BaseProperty baseProperty, ConnectMongoDB connectMongoDB, CollectionNameConvert collectionNameConvert) {
        this.mongoClient = mongoClient;
        this.slaveDataSourceList = slaveDataSourceList;
        this.baseProperty = baseProperty;
        this.connectMongoDB = connectMongoDB;
        this.collectionNameConvert = collectionNameConvert;
    }

    public ExecutorFactory() {
    }

    public static class ExecuteFactoryBuilder {
        private Map<String, MongoCollection<Document>> collectionMap;
        private MongoClient mongoClient;
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

        public ExecuteFactoryBuilder mongoClient(MongoClient mongoClient) {
            this.mongoClient = mongoClient;
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
            return new ExecutorFactory(this.mongoClient, this.slaveDataSourceList, this.baseProperty, this.connectMongoDB, this.collectionNameConvert);
        }

        public String toString() {
            return "ExecuteFactory.ExecuteFactoryBuilder(collectionMap=" + this.collectionMap + ", mongoClient=" + this.mongoClient + ", slaveDataSourceList=" + this.slaveDataSourceList + ", baseProperty=" + this.baseProperty + ", connectMongoDB=" + this.connectMongoDB + ", collectionNameConvert=" + this.collectionNameConvert + ")";
        }
    }

}
