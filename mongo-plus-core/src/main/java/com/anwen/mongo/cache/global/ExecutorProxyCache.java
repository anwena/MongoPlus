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

    public static final Map<String, MethodExecutorStrategy> EXECUTOR_MAP = new HashMap<>(ExecuteMethodEnum.values().length);

    static {
        EXECUTOR_MAP.put(ExecuteMethodEnum.SAVE.getMethod(), new SaveExecutorStrategy());
        EXECUTOR_MAP.put(ExecuteMethodEnum.REMOVE.getMethod(), new RemoveExecutorStrategy());
        EXECUTOR_MAP.put(ExecuteMethodEnum.UPDATE.getMethod(), new UpdateExecutorStrategy());
        EXECUTOR_MAP.put(ExecuteMethodEnum.QUERY.getMethod(), new QueryExecutorStrategy());
        EXECUTOR_MAP.put(ExecuteMethodEnum.AGGREGATE.getMethod(), new AggregateExecutorStrategy());
        EXECUTOR_MAP.put(ExecuteMethodEnum.COUNT.getMethod(), new CountExecutorStrategy());
        EXECUTOR_MAP.put(ExecuteMethodEnum.BULK_WRITE.getMethod(), new BulkWriteExecutorStrategy());
    }

}
