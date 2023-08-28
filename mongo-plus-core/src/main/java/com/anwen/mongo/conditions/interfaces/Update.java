package com.anwen.mongo.conditions.interfaces;

import com.anwen.mongo.support.SFunction;

import java.io.Serializable;

public interface Update<Children, T> extends Serializable {

    Children set(boolean condition, SFunction<T,Object> column, Object value);

    Children set(SFunction<T,Object> column, Object value);

    Children set(boolean condition, String column, Object value);

    Children set(String column, Object value);

}
