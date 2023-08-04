package com.anwen.mongo.sql.query;

import com.anwen.mongo.enums.OrderEnum;
import com.anwen.mongo.sql.conditions.AbstractChainWrapper;
import com.anwen.mongo.sql.conditions.interfaces.Query;
import com.anwen.mongo.sql.conditions.interfaces.aggregate.project.Projection;
import com.anwen.mongo.sql.support.SFunction;

public class QueryChainWrapper<T,Children extends AbstractChainWrapper<T,Children>> extends AbstractChainWrapper<T,Children> implements Query<T,Children> {

    @Override
    public Children projection(Projection... projection) {
        return getBaseProject(projection);
    }

    @SafeVarargs
    @Override
    public final Children projectionDisplay(SFunction<T, Object>... column) {
        return getBaseProjectDisplay(column);
    }

    @Override
    public Children projectionDisplay(String... column) {
        return getBaseProjectDisplay(column);
    }

    @SafeVarargs
    @Override
    public final Children projectionNone(SFunction<T, Object>... column) {
        return getBaseProjectNone(column);
    }

    @Override
    public Children projectionNone(String... column) {
        return getBaseProjectNone(column);
    }

    @Override
    public Children projection(boolean displayId, Projection... projection) {
        getBaseProject(projection);
        return displayId ? typedThis : setProjectNoneId();
    }

    @SafeVarargs
    @Override
    public final Children projectionDisplay(boolean displayId, SFunction<T, Object>... column) {
        getBaseProjectDisplay(column);
        return displayId ? typedThis : setProjectNoneId();
    }

    @Override
    public Children projectionDisplay(boolean displayId, String... column) {
        getBaseProjectDisplay(column);
        return displayId ? typedThis : setProjectNoneId();
    }

    @SafeVarargs
    @Override
    public final Children projectionNone(boolean displayId, SFunction<T, Object>... column) {
        getBaseProjectNone(column);
        return displayId ? typedThis : setProjectNoneId();
    }

    @Override
    public Children projectionNone(boolean displayId, String... column) {
        getBaseProjectNone(column);
        return displayId ? typedThis : setProjectNoneId();
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
