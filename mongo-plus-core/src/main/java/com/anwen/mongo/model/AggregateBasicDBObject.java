package com.anwen.mongo.model;

import com.mongodb.BasicDBObject;
import com.mongodb.DBObjectCodec;
import org.bson.UuidRepresentation;
import org.bson.codecs.Codec;
import org.bson.codecs.Decoder;
import org.bson.codecs.DecoderContext;
import org.bson.json.JsonReader;

import java.util.Map;

import static org.bson.codecs.configuration.CodecRegistries.withUuidRepresentation;

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
