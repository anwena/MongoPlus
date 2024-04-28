package com.anwen.mongo.cache.global;

import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.proxy.executor.MethodExecutor;
import com.anwen.mongo.proxy.executor.impl.AggregateExecutor;
import com.anwen.mongo.proxy.executor.impl.BulkWriteExecutor;
import com.anwen.mongo.proxy.executor.impl.CountExecutor;
import com.anwen.mongo.proxy.executor.impl.QueryExecutor;
import com.anwen.mongo.proxy.executor.impl.RemoveExecutor;
import com.anwen.mongo.proxy.executor.impl.SaveExecutor;
import com.anwen.mongo.proxy.executor.impl.UpdateExecutor;

import java.util.HashMap;
import java.util.Map;

/**
 * 方法执行器缓存
 *
 * @author loser
 * @date 2024/4/28
 */
public class ExecutorProxyCache {

    public static final Map<String, MethodExecutor> EXECUTOR_MAP = new HashMap<>(ExecuteMethodEnum.values().length);

    static {
        EXECUTOR_MAP.put(ExecuteMethodEnum.SAVE.getMethod(), new SaveExecutor());
        EXECUTOR_MAP.put(ExecuteMethodEnum.REMOVE.getMethod(), new RemoveExecutor());
        EXECUTOR_MAP.put(ExecuteMethodEnum.UPDATE.getMethod(), new UpdateExecutor());
        EXECUTOR_MAP.put(ExecuteMethodEnum.QUERY.getMethod(), new QueryExecutor());
        EXECUTOR_MAP.put(ExecuteMethodEnum.AGGREGATE.getMethod(), new AggregateExecutor());
        EXECUTOR_MAP.put(ExecuteMethodEnum.COUNT.getMethod(), new CountExecutor());
        EXECUTOR_MAP.put(ExecuteMethodEnum.BULK_WRITE.getMethod(), new BulkWriteExecutor());
    }

}
