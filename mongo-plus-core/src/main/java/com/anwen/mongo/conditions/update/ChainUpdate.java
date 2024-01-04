package com.anwen.mongo.conditions.update;

import com.mongodb.client.ClientSession;

/**
 * 修改方法定义
 * @author JiaChaoYang
 * @date 2023/6/24/024 2:58
*/
public interface ChainUpdate {

    boolean update();

    @Deprecated
    boolean update(ClientSession clientSession);

    boolean remove();

    @Deprecated
    boolean remove(ClientSession clientSession);

}
