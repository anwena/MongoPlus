package com.anwen.mongo.execute.instance;

import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.execute.Execute;
import com.anwen.mongo.model.BaseProperty;
import com.mongodb.client.MongoClient;

import java.util.Map;

/**
 * 扩展执行器类，自定义执行器应该继承该类，但只能拿到最终条件
 *
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2023-12-28 15:57
 **/
public abstract class ExpandExecute extends Execute {

    /**
     * 拓展参数，可能为null，取决执行器工厂的获取实例方法
     * @author JiaChaoYang
     * @date 2023/12/28 16:02
    */
    public Map<Object,Object> expandExecuteMap;

    public ExpandExecute(MongoClient mongoClient, BaseProperty baseProperty, CollectionNameConvert collectionNameConvert, CollectionManager collectionManager, Map<Object, Object> expandExecuteMap) {
        super(mongoClient, baseProperty, collectionNameConvert, collectionManager);
        this.expandExecuteMap = expandExecuteMap;
    }
}
