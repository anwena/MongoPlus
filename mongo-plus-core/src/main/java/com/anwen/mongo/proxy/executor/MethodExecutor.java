package com.anwen.mongo.proxy.executor;

import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.interceptor.Interceptor;

/**
 * 解耦逻辑
 *
 * @author loser
 * @date 2024/4/28
 */
public interface MethodExecutor {

    /**
     * 方法类型
     */
    ExecuteMethodEnum method();

    /**
     * 执行拦截方法
     */
    void invoke(Interceptor interceptor, Object[] args);

}
