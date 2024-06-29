package com.anwen.mongo.logic;

import com.anwen.mongo.aware.impl.NamespaceAware;
import com.anwen.mongo.cache.global.CollectionLogicDeleteCache;

/**
 * 逻辑删除链接命名空间感知类
 *
 * @author loser
 * @date 2024/6/29
 */
public class LogicNamespaceAware implements NamespaceAware {

    @Override
    public void nameSpaceAware(Namespace namespace) {
        String fullName = namespace.getDataBase() + "." + namespace.getCollectionName();
        CollectionLogicDeleteCache.mapperClassByCollection(fullName, namespace.getEntityClass());
    }

}
