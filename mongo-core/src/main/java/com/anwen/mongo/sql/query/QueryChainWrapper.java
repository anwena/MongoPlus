package com.anwen.mongo.sql.query;

import com.anwen.mongo.enums.LogicTypeEnum;
import com.anwen.mongo.sql.conditions.AbstractChainWrapper;
import com.anwen.mongo.sql.conditions.interfaces.Query;
import com.anwen.mongo.sql.support.SFunction;

public class QueryChainWrapper<T,Children extends AbstractChainWrapper<T,Children>> extends AbstractChainWrapper<T,Children> implements Query<T,Children> {
    @Override
    public Children select(SFunction<T, Object> column) {
        return null;
    }

}
