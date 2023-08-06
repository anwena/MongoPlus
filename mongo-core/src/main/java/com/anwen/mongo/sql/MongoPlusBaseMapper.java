package com.anwen.mongo.sql;

/**
 * @author JiaChaoYang
 **/
public class MongoPlusBaseMapper<T> extends ServiceImpl<T> implements IService<T> {

    public MongoPlusBaseMapper(SqlOperation<?> sqlOperation) {
        super.setSqlOperation((SqlOperation<T>) sqlOperation);
    }
}
