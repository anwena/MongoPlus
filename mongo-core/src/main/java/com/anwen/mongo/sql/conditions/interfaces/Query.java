package com.anwen.mongo.sql.conditions.interfaces;

import com.anwen.mongo.sql.query.LambdaQueryChainWrapper;
import com.anwen.mongo.sql.support.SFunction;

public interface Query<T,Children> {

    Children select(SFunction<T,Object> column);

}
