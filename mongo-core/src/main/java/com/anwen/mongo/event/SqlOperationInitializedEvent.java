package com.anwen.mongo.event;


import com.anwen.mongo.execute.SqlOperation;
import lombok.Getter;
import org.springframework.context.ApplicationEvent;

/**
 * 自定义事件，通知其他类sqlOperation已经初始化完毕
 * @author JiaChaoYang
 * @date 2023/6/26/026 22:10
*/ 
@Getter
public class SqlOperationInitializedEvent extends ApplicationEvent {

    /**
     * -- GETTER --
     *  获取事件
     *
     */
    private final SqlOperation<?> sqlOperation;

    /**
     * 初始化事件
     * @author JiaChaoYang
     * @date 2023/6/26/026 22:09
    */ 
    public SqlOperationInitializedEvent(SqlOperation<?> sqlOperation) {
        super(sqlOperation);
        this.sqlOperation = sqlOperation;
    }

}
