package com.anwen.mongo.sql.conditions.query;

import com.anwen.mongo.sql.model.PageParam;
import com.anwen.mongo.sql.model.PageResult;

import java.util.List;

/**
 * 查询方法定义
 * @author JiaChaoYang
 * @date 2023/6/24/024 2:01
*/
public interface ChainQuery<T> {

    List<T> list();

    T one();

    PageResult<T> page(PageParam pageParam);

    PageResult<T> page(Integer pageNum, Integer pageSize);

}
