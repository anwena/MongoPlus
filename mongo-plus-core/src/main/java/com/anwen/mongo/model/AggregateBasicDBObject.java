package com.anwen.mongo.model;

import com.mongodb.BasicDBObject;

import java.util.Map;

/**
 * 继承BasicDBObject，实现增加排序
 * @author JiaChaoYang
 * @date 2024-01-10 09:51
 **/
public class AggregateBasicDBObject extends BasicDBObject {

    private Integer order;

    public Integer getOrder() {
        return order;
    }

    public void setOrder(Integer order) {
        this.order = order;
    }

    public AggregateBasicDBObject(int size, Integer order) {
        super(size);
        this.order = order;
    }

    public AggregateBasicDBObject(String key, Object value, Integer order) {
        super(key, value);
        this.order = order;
    }

    public AggregateBasicDBObject(Map map, Integer order) {
        super(map);
        this.order = order;
    }
}
