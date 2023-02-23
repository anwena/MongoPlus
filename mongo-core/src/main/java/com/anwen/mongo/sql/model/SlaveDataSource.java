package com.anwen.mongo.sql.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * @Description:
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.sql.model
 * @Author: JiaChaoYang
 * @CreateTime: 2023-02-18 15:05
 * @Version: 1.0
 */
@EqualsAndHashCode(callSuper = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
public class SlaveDataSource extends BaseProperty{
    /**
     * 数据源名称
     **/
    private String slaveName;
}
