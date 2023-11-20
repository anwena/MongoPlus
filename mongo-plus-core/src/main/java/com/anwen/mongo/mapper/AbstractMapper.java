package com.anwen.mongo.mapper;

import com.anwen.mongo.execute.SqlExecute;

import java.io.Serializable;

/**
 * @Description:
 * @Name: AbstractMapper
 * @Author: Bomber
 * @CreateTime: 2023/11/20 10:35
 */
public class AbstractMapper<T> implements BaseMapper<T> {

    protected SqlExecute sqlExecute;
    protected Class<?> clazz = Object.class;

    public void setSqlExecute(SqlExecute sqlExecute) {
        sqlExecute.init(clazz);
        this.sqlExecute = sqlExecute;
    }

    @Override
    public T getById(Serializable id) {
        return (T) sqlExecute.doGetById(id,clazz);
    }
}
