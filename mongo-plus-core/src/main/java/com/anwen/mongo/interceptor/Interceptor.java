package com.anwen.mongo.interceptor;

import com.anwen.mongo.model.command.CommandFailed;
import com.anwen.mongo.model.command.CommandStarted;
import com.anwen.mongo.model.command.CommandSucceeded;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description 拦截器，实际上是通过mongo的命令监听器实现，MongoDB团队并不推荐这么做
 * @date 2023-11-22 14:12
 **/
public interface Interceptor {

    /**
     * 最高优先级
     * @see java.lang.Integer#MIN_VALUE
     */
    int HIGHEST_PRECEDENCE = Integer.MIN_VALUE;

    /**
     * 最低优先级
     * @see java.lang.Integer#MAX_VALUE
     */
    int LOWEST_PRECEDENCE = Integer.MAX_VALUE;

    /**
     * 处理命令开始信息
     * @param commandStarted 命令执行开始信息对象
     * @author JiaChaoYang
     * @date 2023/11/22 14:34
    */
    void commandStarted(CommandStarted commandStarted);

    /**
     * 处理命令成功信息
     * @param commandSucceeded 命令成功信息对象
     * @author JiaChaoYang
     * @date 2023/11/22 14:35
    */
    void commandSucceeded(CommandSucceeded commandSucceeded);

    /**
     * 处理命令失败信息
     * @param commandFailed 处理命令失败信息对象
     * @author JiaChaoYang
     * @date 2023/11/22 14:35
    */
    void commandFailed(CommandFailed commandFailed);

    /**
     * 指定拦截器排序
     * @return int
     * @author JiaChaoYang
     * @date 2023/11/22 16:27
    */
    int getOrder();

}
