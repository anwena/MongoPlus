package com.anwen.mongo.conditions.interfaces;

import com.anwen.mongo.support.SFunction;

import java.io.Serializable;
import java.util.List;

public interface Update<T , Children> extends Serializable {

    Children set(boolean condition, SFunction<T,Object> column, Object value);

    Children set(SFunction<T,Object> column, Object value);

    Children set(boolean condition, String column, Object value);

    Children set(String column, Object value);

    Children setOnInsert(boolean condition, SFunction<T,Object> column, Object value);

    Children setOnInsert(SFunction<T,Object> column, Object value);

    Children setOnInsert(boolean condition, String column, Object value);

    Children setOnInsert(String column, Object value);

    Children push(boolean condition,SFunction<T,Object> column,Object value);

    Children push(SFunction<T,Object> column,Object value);

    Children push(boolean condition,String column,Object value);

    Children push(String column,Object value);

    Children push(boolean condition,SFunction<T,Object> column,Object ... value);

    Children push(SFunction<T,Object> column,Object ... value);

    Children push(boolean condition,String column,Object ... value);

    Children push(String column,Object ... value);

    Children push(boolean condition, SFunction<T,Object> column, List<?> value);

    Children push(SFunction<T,Object> column, List<?> value);

    Children push(boolean condition, String column, List<?> value);

    Children push(String column, List<?> value);

    Children inc(boolean condition,SFunction<T,Object> column,Number value);

    Children inc(SFunction<T,Object> column,Number value);

    Children inc(boolean condition,String column,Number value);

    Children inc(String column,Number value);

}
