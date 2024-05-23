package com.anwen.mongo.bson;

import com.anwen.mongo.support.SFunction;
import com.mongodb.BasicDBObject;

/**
 * 支持lambda的BasicDBObject
 * @author JiaChaoYang
 * @date 2023-11-14 15:01
 **/
public class MongoPlusBasicDBObject extends BasicDBObject {

    public <T,R> void put(SFunction<T,R> key,BasicDBObject value){
        put(key.getFieldNameLine(),value);
    }

    public <T,R> void append(SFunction<T,R> key,BasicDBObject value){
        super.append(key.getFieldNameLine(),value);
    }

    public <T,R> void get(SFunction<T,R> key){
        super.get(key.getFieldNameLine());
    }

    public void put(String key,BasicDBObject value){
        if (containsKey(key)){
            super.put(key,new BasicDBObject((BasicDBObject) get(key)){{
                value.keySet().forEach(basic -> {
                    append(basic,value.get(basic));
                });
            }});
        }else {
            super.put(key,value);
        }
    }

}
