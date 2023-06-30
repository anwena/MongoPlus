package com.anwen.mongo.sql.conditions.update;

/**
 * 修改方法定义
 * @author JiaChaoYang
 * @date 2023/6/24/024 2:58
*/
public interface ChainUpdate {

    boolean update();

    boolean remove();

}
