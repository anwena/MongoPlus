package com.anwen.mongo.context;

import com.mongodb.client.ClientSession;

import java.util.Map;

/**
 * 使用spring中的事务
 * @author JiaChaoYang
 **/
public class MongoTransactionSpring {

    private static final ThreadLocal<Map<Object, Object>> resources =
            new ThreadLocal<>();

    private static final ThreadLocal<String> currentTransactionName =
            new ThreadLocal<>();

    public static void setResources(Map<Object, Object> resource){
        resources.set(resource);
    }

    public static Map<Object, Object> getResourceMap(){
        return resources.get();
    }

    public static ClientSession getResource(Object key){
        return (ClientSession) resources.get().get(key);
    }

    public static ClientSession getResource(){
        if (currentTransactionName.get() != null){
            return (ClientSession) resources.get().get(currentTransactionName.get());
        }
        return null;
    }

    public static void setCurrentTransactionName(String name) {
        currentTransactionName.set(name);
    }

    public static String getCurrentTransactionName() {
        return currentTransactionName.get();
    }

    public static void clear(){
        resources.remove();
        currentTransactionName.remove();
    }

}
