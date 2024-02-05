package com.anwen.mongo.execute;

import com.anwen.mongo.context.MongoTransactionContext;
import com.anwen.mongo.execute.inject.InjectAbstractExecute;
import com.anwen.mongo.execute.instance.DefaultExecute;
import com.anwen.mongo.execute.instance.SessionExecute;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.proxy.ExecutorProxy;
import com.mongodb.client.ClientSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Proxy;
import java.util.Optional;

/**
 * 执行器工厂
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2023-12-28 10:55
 **/
public class ExecutorFactory {

    private final Logger logger = LoggerFactory.getLogger(ExecutorFactory.class);

    private MongoPlusClient mongoPlusClient;

    public ExecutorFactory() {
    }

    public ExecutorFactory(MongoPlusClient mongoPlusClient){
        this.mongoPlusClient = mongoPlusClient;
    }

    public Execute getExecute(){
        ClientSession clientSessionContext = MongoTransactionContext.getClientSessionContext();
        Execute execute = Optional.ofNullable(clientSessionContext)
                .map(clientSession -> (Execute) new SessionExecute(clientSession))
                .orElseGet(DefaultExecute::new);
        Class<? extends Execute> clazz = execute.getClass();
        return (Execute) Proxy.newProxyInstance(clazz.getClassLoader(),clazz.getInterfaces(),new ExecutorProxy(execute));

    }

    public InjectAbstractExecute getInjectExecute(String database){
        ClientSession clientSessionContext = MongoTransactionContext.getClientSessionContext();
        Execute execute = Optional.ofNullable(clientSessionContext)
                .map(clientSession -> (Execute) new SessionExecute(clientSession))
                .orElseGet(DefaultExecute::new);
        Class<? extends Execute> clazz = execute.getClass();
        return new InjectAbstractExecute(mongoPlusClient.getCollectionManager(database), (Execute) Proxy.newProxyInstance(clazz.getClassLoader(),clazz.getInterfaces(),new ExecutorProxy(execute)));
    }

}
