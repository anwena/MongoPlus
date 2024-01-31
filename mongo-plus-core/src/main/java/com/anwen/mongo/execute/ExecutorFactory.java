package com.anwen.mongo.execute;

import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.context.MongoTransactionContext;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.execute.inject.InjectAbstractExecute;
import com.anwen.mongo.execute.instance.DefaultExecute;
import com.anwen.mongo.execute.instance.SessionExecute;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.model.BaseProperty;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.client.ClientSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.Objects;

/**
 * 执行器工厂
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2023-12-28 10:55
 **/
public class ExecutorFactory {

    private final Logger logger = LoggerFactory.getLogger(ExecutorFactory.class);

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

    private MongoPlusClient mongoPlusClient;

    /**
     * 获取执行器
     * @author JiaChaoYang
     * @date 2023/12/28 14:49
    */
    public AbstractExecute getExecute(String database) {
        return getExecute(getCollectionManager(database));
    }

    /**
     * 获取执行器
     * @author JiaChaoYang
     * @date 2024/1/20 23:32
    */
    public Execute getExecuteInterface(String database){
        return getExecute(getCollectionManager(database));
    }

    public AbstractExecute getExecute(CollectionManager collectionManager){
        ClientSession clientSessionContext = MongoTransactionContext.getClientSessionContext();
        if (clientSessionContext != null) {
            return new SessionExecute(collectionNameConvert,collectionManager,clientSessionContext);
        }
        return new DefaultExecute(collectionNameConvert,collectionManager);
    }

    public InjectAbstractExecute getInjectExecute(String database){
        CollectionManager collectionManager = getCollectionManager(database);
        return new InjectAbstractExecute(collectionManager,getExecute(collectionManager));
    }

    public CollectionManager getCollectionManager(String database){
        Map<String, CollectionManager> managerMap = mongoPlusClient.getCollectionManager();
        if (StringUtils.isBlank(database)){
            database = managerMap.keySet().stream().findFirst().get();
        }
        CollectionManager collectionManager = managerMap.get(database);
        if (null == collectionManager){
            collectionManager = new CollectionManager(mongoPlusClient.getMongoClient(), collectionNameConvert, database);
            mongoPlusClient.getMongoDatabase().add(mongoPlusClient.getMongoClient().getDatabase(database));
            mongoPlusClient.getCollectionManager().put(database,collectionManager);
        }
        return collectionManager;
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

    public BaseProperty getBaseProperty() {
        return this.baseProperty;
    }

    public CollectionNameConvert getCollectionNameConvert() {
        return this.collectionNameConvert;
    }

    public void setBaseProperty(BaseProperty baseProperty) {
        this.baseProperty = baseProperty;
    }

    public void setCollectionNameConvert(CollectionNameConvert collectionNameConvert) {
        this.collectionNameConvert = collectionNameConvert;
    }

    public void setMongoPlusClient(MongoPlusClient mongoPlusClient){
        this.mongoPlusClient = mongoPlusClient;
    }

    public MongoPlusClient getMongoPlusClient(){
        return this.mongoPlusClient;
    }

    protected boolean canEqual(Object other) {
        return other instanceof ExecutorFactory;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ExecutorFactory)) return false;
        ExecutorFactory that = (ExecutorFactory) o;
        return Objects.equals(logger, that.logger) && Objects.equals(getBaseProperty(), that.getBaseProperty()) && Objects.equals(getCollectionNameConvert(), that.getCollectionNameConvert()) && Objects.equals(getMongoPlusClient(), that.getMongoPlusClient());
    }

    @Override
    public int hashCode() {
        return Objects.hash(logger, getBaseProperty(), getCollectionNameConvert(), getMongoPlusClient());
    }

    @Override
    public String toString() {
        return "ExecutorFactory{" +
                "baseProperty=" + baseProperty +
                ", collectionNameConvert=" + collectionNameConvert +
                ", mongoPlusClient=" + mongoPlusClient +
                '}';
    }

    public ExecutorFactory(BaseProperty baseProperty, CollectionNameConvert collectionNameConvert, MongoPlusClient mongoPlusClient) {
        this.baseProperty = baseProperty;
        this.collectionNameConvert = collectionNameConvert;
        this.mongoPlusClient = mongoPlusClient;
    }

    public ExecutorFactory() {
    }

    public static class ExecuteFactoryBuilder {
        private BaseProperty baseProperty;
        private CollectionNameConvert collectionNameConvert;

        private MongoPlusClient mongoPlusClient;

        ExecuteFactoryBuilder() {
        }

        public ExecuteFactoryBuilder baseProperty(BaseProperty baseProperty) {
            this.baseProperty = baseProperty;
            return this;
        }

        public ExecuteFactoryBuilder collectionNameConvert(CollectionNameConvert collectionNameConvert) {
            this.collectionNameConvert = collectionNameConvert;
            return this;
        }

        public ExecuteFactoryBuilder mongoPlusClient(MongoPlusClient mongoPlusClient){
            this.mongoPlusClient = mongoPlusClient;
            return this;
        }

        public ExecutorFactory build() {
            return new ExecutorFactory(this.baseProperty, this.collectionNameConvert, mongoPlusClient);
        }
    }

}
