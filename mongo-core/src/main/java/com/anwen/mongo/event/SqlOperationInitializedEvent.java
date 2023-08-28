package com.anwen.mongo.event;


import com.anwen.mongo.execute.SqlOperation;
import lombok.Getter;

/**
 * 自定义事件，通知其他类sqlOperation已经初始化完毕
 * @author JiaChaoYang
 * @date 2023/6/26/026 22:10
*/ 
public class SqlOperationInitializedEvent extends ApplicationEvent {

    /**
     *  获取事件
     */
    private final SqlOperation<?> sqlOperation;

    /**
     * 初始化事件
     * @author JiaChaoYang
     * @date 2023/6/26/026 22:09
    */ 
    public SqlOperationInitializedEvent(SqlOperation<?> sqlOperation) {
        this.sqlOperation = sqlOperation;
    }

    public SqlOperation<?> getSqlOperation() {
        return sqlOperation;
    }
}
