package com.anwen.mongo.conditions.query;

import com.anwen.mongo.conditions.AbstractChainWrapper;
import com.anwen.mongo.conditions.interfaces.Query;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.enums.OrderEnum;
import com.anwen.mongo.support.SFunction;

import java.util.List;

/**
 * AbstractChainWrapper的条件扩展类，查询专有的条件，使用类构造条件时，使用QueryChainWrapper的子类，{@link QueryWrapper}
 * @author JiaChaoYang
 * @date 2024/2/3 13:10
*/
public abstract class QueryChainWrapper<T,Children extends QueryChainWrapper<T,Children>> extends AbstractChainWrapper<T,Children> implements Query<T,Children> {

    @Override
    public Children project(Projection... projection) {
        return getBaseProject(projection);
    }

    @Override
    public Children project(List<Projection> projectionList) {
        return getBaseProject(projectionList.toArray(new Projection[0]));
    }

    @SafeVarargs
    @Override
    public final Children projectDisplay(SFunction<T, Object>... column) {
        return getBaseProjectDisplay(column);
    }

    @Override
    public Children projectDisplay(String... column) {
        return getBaseProjectDisplay(column);
    }

    @SafeVarargs
    @Override
    public final Children projectNone(SFunction<T, Object>... column) {
        return getBaseProjectNone(column);
    }

    @Override
    public Children projectNone(String... column) {
        return getBaseProjectNone(column);
    }

    @Override
    public Children project(boolean displayId, Projection... projection) {
        getBaseProject(projection);
        return displayId ? typedThis : setProjectNoneId();
    }

    @SafeVarargs
    @Override
    public final Children projectDisplay(boolean displayId, SFunction<T, Object>... column) {
        getBaseProjectDisplay(column);
        return displayId ? typedThis : setProjectNoneId();
    }

    @Override
    public Children projectDisplay(boolean displayId, String... column) {
        getBaseProjectDisplay(column);
        return displayId ? typedThis : setProjectNoneId();
    }

    @SafeVarargs
    @Override
    public final Children projectNone(boolean displayId, SFunction<T, Object>... column) {
        getBaseProjectNone(column);
        return displayId ? typedThis : setProjectNoneId();
    }

    @Override
    public Children projectNone(boolean displayId, String... column) {
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

    @Override
    public Children limit(long limit) {
        return null;
    }

    @Override
    public Children skip(long skip) {
        return null;
    }

}
