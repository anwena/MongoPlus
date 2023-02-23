package com.anwen.mongo.sql.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description
 * @date 2023-02-23 10:57
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class PageResult<T> {
    /**
     * 当前页
     * @author JiaChaoYang
     * @date 2023/2/23 10:58
    */
    private long pageNum;

    /**
     * 每页显示行数
     * @author JiaChaoYang
     * @date 2023/2/23 10:58
    */
    private long pageSize;

    /**
     * 总行数
     * @author JiaChaoYang
     * @date 2023/2/23 10:58
    */
    private long totalSize;

    /**
     * 页码总数
     * @author JiaChaoYang
     * @date 2023/2/23 10:58
    */
    private long totalPages;

    /**
     * 数据模型
     * @author JiaChaoYang
     * @date 2023/2/23 10:59
    */
    private List<T> contentData;
}
