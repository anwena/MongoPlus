package com.anwen.mongo.conditions.update;

import com.anwen.mongo.conditions.AbstractChainWrapper;
import com.anwen.mongo.conditions.interfaces.Update;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.support.SFunction;

import java.util.ArrayList;
import java.util.List;

/**
 * update接口实现
 * @author JiaChaoYang
 * @date 2023/6/24/024 12:45
*/
public class UpdateChainWrapper<T,Children extends UpdateChainWrapper<T,Children>> extends AbstractChainWrapper<T, Children> implements Update<T,Children> {

    @SuppressWarnings("unchecked")
    protected final Children typedThis = (Children) this;

    private final List<CompareCondition> updateCompareList = new ArrayList<>();

    public List<CompareCondition> getUpdateCompareList() {
        return updateCompareList;
    }

    @Override
    public Children set(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? set(column,value) : typedThis;
    }

    @Override
    public Children set(SFunction<T, Object> column, Object value) {
        return getBaseUpdateCompare(column,value);
    }

    @Override
    public Children set(boolean condition, String column, Object value) {
        return condition ? set(column,value) : typedThis;
    }

    @Override
    public Children set(String column, Object value) {
        return getBaseUpdateCompare(column,value);
    }

    @Override
    public Children setOnInsert(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? set(column,value) : typedThis;
    }

    @Override
    public Children setOnInsert(SFunction<T, Object> column, Object value) {
        return getBaseUpdateCompare(column,value);
    }

    @Override
    public Children setOnInsert(boolean condition, String column, Object value) {
        return condition ? set(column,value) : typedThis;
    }

    @Override
    public Children setOnInsert(String column, Object value) {
        return getBaseUpdateCompare(column,value);
    }

    @Override
    public Children push(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? push(column,value) : typedThis;
    }

    @Override
    public Children push(SFunction<T, Object> column, Object value) {
        return getBaseUpdateCompare(column,value);
    }

    @Override
    public Children push(boolean condition, String column, Object value) {
        return condition ? push(column,value) : typedThis;
    }

    @Override
    public Children push(String column, Object value) {
        return getBaseUpdateCompare(column,value);
    }

    @Override
    public Children push(boolean condition, SFunction<T, Object> column, Object... value) {
        return condition ? push(column,value) : typedThis;
    }

    @Override
    public Children push(SFunction<T, Object> column, Object... value) {
        for (Object o : value) {
            getBaseUpdateCompare(column,o);
        }
        return typedThis;
    }

    @Override
    public Children push(boolean condition, String column, Object... value) {
        return condition ? push(column,value) : typedThis;
    }

    @Override
    public Children push(String column, Object... value) {
        for (Object o : value) {
            getBaseUpdateCompare(column,o);
        }
        return typedThis;
    }

    @Override
    public Children push(boolean condition, SFunction<T, Object> column, List<?> value) {
        return condition ? push(column,value) : typedThis;
    }

    @Override
    public Children push(SFunction<T, Object> column, List<?> value) {
        for (Object o : value) {
            getBaseUpdateCompare(column,o);
        }
        return typedThis;
    }

    @Override
    public Children push(boolean condition, String column, List<?> value) {
        return condition ? push(column,value) : typedThis;
    }

    @Override
    public Children push(String column, List<?> value) {
        for (Object o : value) {
            getBaseUpdateCompare(column,o);
        }
        return typedThis;
    }

    private Children getBaseUpdateCompare(SFunction<T, Object> column, Object value){
        updateCompareList.add(new CompareCondition(Thread.currentThread().getStackTrace()[2].getMethodName(),column.getFieldNameLine(),value,column.getImplClass(),column.getField()));
        return typedThis;
    }

    private Children getBaseUpdateCompare(String column, Object value){
        updateCompareList.add(new CompareCondition(Thread.currentThread().getStackTrace()[2].getMethodName(),column,value,Object.class,null));
        return typedThis;
    }

}
