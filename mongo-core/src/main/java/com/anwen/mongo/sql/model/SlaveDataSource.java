package com.anwen.mongo.sql.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @Description:
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.sql.model
 * @Author: JiaChaoYang
 * @CreateTime: 2023-02-18 15:05
 * @Version: 1.0
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class SlaveDataSource {
    /**
     * 数据源名称
     **/
    private String slaveName;
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
}
