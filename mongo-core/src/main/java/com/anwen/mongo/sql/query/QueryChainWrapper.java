package com.anwen.mongo.sql.query;

import com.anwen.mongo.enums.OrderEnum;
import com.anwen.mongo.sql.conditions.AbstractChainWrapper;
import com.anwen.mongo.sql.conditions.interfaces.Query;
import com.anwen.mongo.sql.support.SFunction;

public class QueryChainWrapper<T,Children extends AbstractChainWrapper<T,Children>> extends AbstractChainWrapper<T,Children> implements Query<T,Children> {
    @Override
    public Children select(SFunction<T, Object> column) {
        return null;
    }

    @Override
    public Children orderByAsc(SFunction<T, Object> column) {
        return getBaseOrder(OrderEnum.ORDER_BY.getFlag(), column);
    }

    @Override
    public Children orderByDesc(SFunction<T, Object> column) {
        return getBaseOrder(OrderEnum.ORDER_BY_DESC.getFlag(), column);
    }

    @Override
    public Children orderByAsc(String column) {
        return getBaseOrder(OrderEnum.ORDER_BY.getFlag(), column);
    }

    @Override
    public Children orderByDesc(String column) {
        return getBaseOrder(OrderEnum.ORDER_BY_DESC.getFlag(), column);
    }

}
