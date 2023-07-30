package com.anwen.mongo.sql.conditions.interfaces.aggregate;

import java.io.Serializable;

/**
 * 管道操作符
 *
 * @author JiaChaoYang
 **/
public interface Aggregate<Children, T> extends Serializable {

    Children project();

}
