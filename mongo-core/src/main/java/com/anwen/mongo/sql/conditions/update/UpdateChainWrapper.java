package com.anwen.mongo.sql.conditions.update;

import com.anwen.mongo.sql.conditions.AbstractChainWrapper;
import com.anwen.mongo.sql.conditions.interfaces.Update;
import com.anwen.mongo.sql.interfaces.CompareCondition;
import com.anwen.mongo.sql.support.SFunction;

import java.util.ArrayList;
import java.util.List;

/**
 * update接口实现
 * @author JiaChaoYang
 * @date 2023/6/24/024 12:45
*/
public class UpdateChainWrapper<T,Children extends UpdateChainWrapper<T,Children>> extends AbstractChainWrapper<T,UpdateChainWrapper<T,Children>> implements Update<UpdateChainWrapper<T,Children>,T> {

    protected final Children typedThis = (Children) this;

    private List<CompareCondition> updateCompareList = new ArrayList<>();

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

    private Children getBaseUpdateCompare(SFunction<T, Object> column, Object value){
        updateCompareList.add(new CompareCondition(new Throwable().getStackTrace()[1].getMethodName(), column.getFieldNameLine(),value,0));
        return typedThis;
    }

    private Children getBaseUpdateCompare(String column, Object value){
        updateCompareList.add(new CompareCondition(new Throwable().getStackTrace()[1].getMethodName(), column,value,0));
        return typedThis;
    }

}
