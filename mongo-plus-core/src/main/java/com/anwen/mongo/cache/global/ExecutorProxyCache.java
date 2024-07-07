package com.anwen.mongo.cache.global;

import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.strategy.executor.MethodExecutorStrategy;
import com.anwen.mongo.strategy.executor.impl.*;

import java.util.HashMap;
import java.util.Map;

/**
 * 方法执行器缓存
 *
 * @author loser
 * @date 2024/4/28
 */
public class ExecutorProxyCache {

    public static final Map<ExecuteMethodEnum, MethodExecutorStrategy> EXECUTOR_MAP = new HashMap<>(ExecuteMethodEnum.values().length);

    static {
        EXECUTOR_MAP.put(ExecuteMethodEnum.SAVE, new SaveExecutorStrategy());
        EXECUTOR_MAP.put(ExecuteMethodEnum.REMOVE, new RemoveExecutorStrategy());
        EXECUTOR_MAP.put(ExecuteMethodEnum.UPDATE_OLD, new UpdateExecutorOldStrategy());
        EXECUTOR_MAP.put(ExecuteMethodEnum.UPDATE, new UpdateExecutorStrategy());
        EXECUTOR_MAP.put(ExecuteMethodEnum.QUERY, new QueryExecutorStrategy());
        EXECUTOR_MAP.put(ExecuteMethodEnum.AGGREGATE_OLD, new AggregateExecutorOldStrategy());
        EXECUTOR_MAP.put(ExecuteMethodEnum.AGGREGATE, new AggregateExecutorStrategy());
        EXECUTOR_MAP.put(ExecuteMethodEnum.COUNT, new CountExecutorStrategy());
        EXECUTOR_MAP.put(ExecuteMethodEnum.BULK_WRITE, new BulkWriteExecutorStrategy());
    }

}
