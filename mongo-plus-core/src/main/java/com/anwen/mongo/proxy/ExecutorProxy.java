package com.anwen.mongo.proxy;

import com.alibaba.fastjson.JSON;
import com.anwen.mongo.execute.Execute;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

/**
 * 执行器代理
 *
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2024-02-05 09:15
 **/
public class ExecutorProxy implements InvocationHandler {

    Logger logger = LoggerFactory.getLogger(ExecutorProxy.class);

    private final Execute target;

    public ExecutorProxy(Execute target) {
        this.target = target;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        logger.info("目标：{}",JSON.toJSONString(proxy));
        logger.info("方法：{}",method);
        logger.info("参数：{}",JSON.toJSONString(args[0]));
        logger.info("参数2：{}",args[1].getClass());
        Object invoke = method.invoke(target, args);
        logger.info("执行结束");
        return invoke;
    }
}
