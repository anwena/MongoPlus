package com.anwen.mongo.sql.model;

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

    private Integer pageNum;

    private Integer pageSize;

}
