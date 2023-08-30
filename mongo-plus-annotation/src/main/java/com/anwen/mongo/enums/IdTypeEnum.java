package com.anwen.mongo.enums;

/**
 * @author JiaChaoYang
 * id生成类型
 * @since 2023-02-13 15:59
 **/
public enum IdTypeEnum {

    /**
     * 生成mongoDB自带的_id
     * @author: JiaChaoYang
     * @date: 2023/6/7 21:32
     **/
    OBJECT_ID(0),

    /**
     * 生成UUID
     * @since 2023/2/13 16:09
    */
    ASSIGN_UUID(1),

    /**
     * ULID是一种比UUID更好的方案，它具有可排序性、可读性、低碰撞率、短且轻量级、安全等优势。
     * 在分布式系统中，使用ULID可以提高数据库查询的效率，同时保证数据的唯一性。
     * 如果你正在构建一个分布式系统，不妨考虑使用ULID来标识你的数据和实体。
     * @author: JiaChaoYang
     * @date: 2023/6/7 21:45
     **/
    ASSIGN_ULID(2),

    /**
     * 生成雪花算法
     * @since 2023/2/13 16:09
    */
    ASSIGN_ID(3),

    /**
     * 生成自增id
     * <p style='color:red'>注：自增id会创建一个counters集合，用来存储当前id，请适当分配权限，新增也会慢那么一点点，着重考虑</p>
     * @author JiaChaoYang
     * @date 2023/8/9 22:39
    */
    AUTO(4)

    ;

    private final int key;

    public int getKey() {
        return key;
    }

    IdTypeEnum(int key) {
        this.key = key;
    }
}
