package com.anwen.mongo.enums;

/**
 * 控制写入安全的级别
 * @author JiaChaoYang
 * @date 2024/5/2 下午5:15
 */
public enum WriteConcernEnum {

    /**
     * 使用此写入关注的写入操作将等待确认，使用服务器上配置的默认写入关注。
     * @author JiaChaoYang
     * @date 2024/5/2 下午5:17
     */
    ACKNOWLEDGED,

    /**
     * 不等待服务器的确认
     * @author JiaChaoYang
     * @date 2024/5/2 下午5:17
     */
    UNACKNOWLEDGED,

    /**
     * 适用于集群架构，要求写入操作已经传递到绝大多数投票节点以及主节点后进行应答
     * @author JiaChaoYang
     * @date 2024/5/2 下午5:18
     */
    MAJORITY,

    /**
     * (应答式写入) 要求确认操作已经传播到指定的单个mongodb实例或副本集主实例(缺省为1)
     * @author JiaChaoYang
     * @date 2024/5/2 下午5:19
     */
    W1,

    /**
     * (用于副本集环境) 等待两个节点确认
     * @author anwen
     * @date 2024/5/2 下午5:20
     */
    W2,

    /**
     * (用于副本集环境) 等待三个节点确认
     * @author anwen
     * @date 2024/5/2 下午5:20
     */
    W3,

    /**
     * 写入操作等待服务器分组提交到磁盘上的日志文件。
     * @author anwen
     * @date 2024/5/2 下午5:23
     */
    JOURNALED,

}
