package com.anwen.mongo.sql.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description
 * @date 2023-02-23 10:42
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class BaseProperty {
    /**
     * mongodb地址
     * @author JiaChaoYang
     * @since 2023/2/10 13:45
     */
    private String host;

    /**
     * mongodb端口
     * @since 2023/2/10 13:45
     */
    private String port;

    /**
     * TODO mongodb数据库名称，第二版本需要多数据源，可随时切换
     * @since 2023/2/10 13:45
     */
    private String database;

    /**
     * 用户名
     * @author JiaChaoYang
     * @date 2023/2/23 10:43
    */
    private String username;

    /**
     * 密码
     * @author JiaChaoYang
     * @date 2023/2/23 10:43
    */
    private String password;
}
