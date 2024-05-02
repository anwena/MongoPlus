package com.anwen.mongo.enums;

import com.mongodb.TransactionOptions;

/**
 * 一致性读策略枚举
 * @author JiaChaoYang
 * @date 2024/5/2 下午5:00
 */
public enum ReadConcernEnum {

    /**
     * 使用服务器默认的读取策略
     * @author JiaChaoYang
     * @date 2024/5/2 下午5:03
     */
    DEFAULT,

    /**
     * 读取所有可用且属于当前分片的数据
     * @author JiaChaoYang
     * @date 2024/5/2 下午5:05
     */
    LOCAL,

    /**
     * 读取在大多数节点上提交完成的数据
     * @author JiaChaoYang
     * @date 2024/5/2 下午5:05
     */
    MAJORITY,

    /**
     * 可线性化读取文档
     * @author JiaChaoYang
     * @date 2024/5/2 下午5:05
     */
    LINEARIZABLE,

    /**
     * 读取所有可用的数据
     * @author JiaChaoYang
     * @date 2024/5/2 下午5:05
     */
    AVAILABLE,

    /**
     * 读取最近快照中的数据
     * @author JiaChaoYang
     * @date 2024/5/2 下午5:05
     */
    SNAPSHOT;
}
