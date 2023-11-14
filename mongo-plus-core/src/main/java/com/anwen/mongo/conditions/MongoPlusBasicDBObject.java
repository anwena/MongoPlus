package com.anwen.mongo.conditions;

import com.mongodb.BasicDBObject;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description
 * @date 2023-11-14 15:01
 **/
public class MongoPlusBasicDBObject extends BasicDBObject {

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
