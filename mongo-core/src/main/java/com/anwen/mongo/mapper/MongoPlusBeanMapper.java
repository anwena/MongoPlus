package com.anwen.mongo.mapper;

import com.anwen.mongo.service.IService;
import com.anwen.mongo.service.impl.ServiceImpl;
import com.anwen.mongo.sql.SqlOperation;

/**
 * @author JiaChaoYang
 **/
public class MongoPlusBeanMapper<T> extends ServiceImpl<T> implements IService<T> {

    public MongoPlusBeanMapper(SqlOperation<?> sqlOperation) {
        super.setSqlOperation((SqlOperation<T>) sqlOperation);
    }
}
