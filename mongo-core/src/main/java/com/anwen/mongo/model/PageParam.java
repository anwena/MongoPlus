package com.anwen.mongo.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description 分页参数,默认为1-10
 * @date 2023-02-23 11:03
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class PageParam {

    /**
     * 当前页
     * @author JiaChaoYang
     * @date 2023/6/20/020 23:55
    */
    private Integer pageNum;

    /**
     * 每页显示行数
     * @author JiaChaoYang
     * @date 2023/6/20/020 23:55
    */
    private Integer pageSize;

}
