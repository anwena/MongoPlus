package com.anwen.mongo.conditions.aggregate;

import com.anwen.mongo.conditions.query.ChainQuery;

import java.util.List;

public interface ChainAggregate<T> extends ChainQuery<T> {

    <E> List<E> list(Class<E> clazz);

}
