package com.anwen.mongo.execute;

import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.context.MongoTransactionContext;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.execute.inject.InjectAbstractExecute;
import com.anwen.mongo.execute.instance.DefaultExecute;
import com.anwen.mongo.execute.instance.SessionExecute;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.mapper.DefaultBaseMapperImpl;
import com.anwen.mongo.model.BaseProperty;
import com.anwen.mongo.proxy.ExecutorProxy;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.client.ClientSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Proxy;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import static com.anwen.mongo.toolkit.StringPool.EMPTY;

/**
 * 执行器工厂
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2023-12-28 10:55
 **/
public class ExecutorFactory {

    private final Logger logger = LoggerFactory.getLogger(ExecutorFactory.class);

    /**
     * 获取执行器
     * @author JiaChaoYang
     * @date 2023/12/28 14:49
    */
/*    public BaseMapper getBaseMapper(String database) {
        return getBaseMapper(getCollectionManager(database));
    }*/

/*    public Execute getExecuteInterface(String database){
        return getBaseMapper(getCollectionManager(database));
    }

    public Execute getExecuteInterface(){
        return getBaseMapper(getCollectionManager(EMPTY));
    }*/

    public Execute getExecute(){
        ClientSession clientSessionContext = MongoTransactionContext.getClientSessionContext();
        Execute execute = Optional.ofNullable(clientSessionContext)
                .map(clientSession -> (Execute) new SessionExecute(clientSession))
                .orElseGet(DefaultExecute::new);
        Class<? extends Execute> clazz = execute.getClass();
        return (Execute) Proxy.newProxyInstance(clazz.getClassLoader(),clazz.getInterfaces(),new ExecutorProxy(execute));

    }

/*    public InjectAbstractExecute getInjectExecute(String database){
        CollectionManager collectionManager = getCollectionManager(database);
        return new InjectAbstractExecute(collectionManager, getBaseMapper(collectionManager));
    }*/

/*    public CollectionManager getCollectionManager(String database){
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
    }*/

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

    public ExecutorFactory() {
    }


}
