package com.anwen.mongo.execute;

import com.anwen.mongo.annotation.collection.CollectionName;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.conn.ConnectMongoDB;
import com.anwen.mongo.context.MongoTransactionContext;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.domain.InitMongoCollectionException;
import com.anwen.mongo.execute.instance.DefaultExecute;
import com.anwen.mongo.execute.instance.SessionExecute;
import com.anwen.mongo.manager.MongoPlusClientManager;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.model.BaseProperty;
import com.anwen.mongo.model.SlaveDataSource;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.MongoException;
import com.mongodb.client.ClientSession;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import org.bson.Document;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

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
     * 连接管理器,结构为：{"dataSourceName":{"database":ConnectionManager(){"collection":MongoCollection}}}
     * @author JiaChaoYang
     * @date 2023/12/28 14:15
    */
    private final Map<String,Map<String,CollectionManager>> collectionManagerMap = new ConcurrentHashMap<>();

    /**
     * mongoClient管理器
     * @author JiaChaoYang
     * @date 2024/1/5 15:36
    */
    private MongoPlusClientManager mongoPlusClientManager;

    /**
     * 初始化工厂
     * @author JiaChaoYang
     * @date 2023/12/28 14:50
    */
    public void init(Class<?> clazz) {
        String collectionName = clazz.getSimpleName().toLowerCase();
        MongoPlusClient mongoPlusClient;
        String dataSourceName = "master";
        final String[] dataBaseName = {"default"};
        if (clazz.isAnnotationPresent(CollectionName.class)) {
            CollectionName annotation = clazz.getAnnotation(CollectionName.class);
            collectionName = annotation.value();
            String dataSource = annotation.dataSource();
            dataBaseName[0] = annotation.database();
            if (StringUtils.isNotBlank(dataSource)){
                dataSourceName = dataSource;
            }
        }
        mongoPlusClient = mongoPlusClientManager.getMongoPlusClient(dataSourceName);
        if (mongoPlusClient == null){
            throw new InitMongoCollectionException("No matching slave data source configured");
        }
        try {
            String finalCollectionName = collectionName;
            final String[] finalDataBaseName = {dataBaseName[0]};
            List<MongoDatabase> mongoDatabaseList = new ArrayList<>();
            mongoPlusClient.setCollectionManager(new LinkedHashMap<String,CollectionManager>(){{
                String database = mongoPlusClient.getBaseProperty().getDatabase();
                List<String> list = Arrays.stream(database.split(",")).collect(Collectors.toList());
                if (Objects.equals(finalDataBaseName[0], "default")){
                    finalDataBaseName[0] = list.get(0);
                }
                list.forEach(db -> {
                    CollectionManager collectionManager = new CollectionManager(mongoPlusClient.getMongoClient(), collectionNameConvert, db);
                    if (Objects.equals(db, finalDataBaseName[0])){
                        ConnectMongoDB connectMongoDB = new ConnectMongoDB(mongoPlusClient.getMongoClient(), db, finalCollectionName);
                        MongoCollection<Document> collection = connectMongoDB.open();
                        mongoDatabaseList.add(connectMongoDB.getMongoDatabase());
                        collectionManager.setCollectionMap(finalCollectionName,collection);
                    }
                    put(db,collectionManager);
                });
            }});
            mongoPlusClient.setMongoDatabase(mongoDatabaseList);
        } catch (MongoException e) {
            logger.error("Failed to connect to MongoDB: {}", e.getMessage(), e);
        }
    }

    /**
     * 获取执行器
     * @author JiaChaoYang
     * @date 2023/12/28 14:49
    */
    public AbstractExecute getExecute(String dataSourceName,String database) {
        ClientSession clientSessionContext = MongoTransactionContext.getClientSessionContext();
        Map<String, CollectionManager> managerMap = mongoPlusClientManager.getMongoPlusClient(dataSourceName).getCollectionManager();
        if (managerMap.keySet().size() <= 1 || StringUtils.isBlank(database)){
            database = managerMap.keySet().stream().findFirst().get();
        }
        CollectionManager collectionManager = managerMap.get(database);
        if (null == collectionManager){
            logger.error("Unknown database '{}'",database);
            throw new MongoException(String.format("Unknown database '%s'",database));
        }
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

    public MongoPlusClientManager getMongoClientManager() {
        return mongoPlusClientManager;
    }

    public void setMongoClientManager(MongoPlusClientManager mongoPlusClientManager) {
        this.mongoPlusClientManager = mongoPlusClientManager;
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

    public ExecutorFactory(List<SlaveDataSource> slaveDataSourceList, BaseProperty baseProperty, CollectionNameConvert collectionNameConvert, MongoPlusClientManager mongoPlusClientManager) {
        this.slaveDataSourceList = slaveDataSourceList;
        this.baseProperty = baseProperty;
        this.collectionNameConvert = collectionNameConvert;
        this.mongoPlusClientManager = mongoPlusClientManager;
    }

    public ExecutorFactory() {
    }

    public static class ExecuteFactoryBuilder {
        private List<SlaveDataSource> slaveDataSourceList;
        private BaseProperty baseProperty;
        private CollectionNameConvert collectionNameConvert;

        private MongoPlusClientManager mongoPlusClientManager;

        ExecuteFactoryBuilder() {
        }

        public ExecuteFactoryBuilder slaveDataSourceList(List<SlaveDataSource> slaveDataSourceList) {
            this.slaveDataSourceList = slaveDataSourceList;
            return this;
        }

        public ExecuteFactoryBuilder baseProperty(BaseProperty baseProperty) {
            this.baseProperty = baseProperty;
            return this;
        }

        public ExecuteFactoryBuilder collectionNameConvert(CollectionNameConvert collectionNameConvert) {
            this.collectionNameConvert = collectionNameConvert;
            return this;
        }

        public ExecuteFactoryBuilder mongoPlusClientManager(MongoPlusClientManager mongoPlusClientManager){
            this.mongoPlusClientManager = mongoPlusClientManager;
            return this;
        }

        public ExecutorFactory build() {
            return new ExecutorFactory(this.slaveDataSourceList, this.baseProperty, this.collectionNameConvert,this.mongoPlusClientManager);
        }
    }

}
