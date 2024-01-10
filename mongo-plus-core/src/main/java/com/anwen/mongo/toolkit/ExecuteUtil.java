package com.anwen.mongo.toolkit;

import com.anwen.mongo.constant.SqlOperationConstant;
import com.mongodb.BasicDBObject;
import com.mongodb.MongoException;
import org.bson.types.ObjectId;

import java.util.Map;

/**
 * @author JiaChaoYang
 **/
public class ExecuteUtil {

    public static BasicDBObject getFilter(Map<String, Object> entityMap) {
        if (!entityMap.containsKey(SqlOperationConstant._ID)) {
            throw new MongoException("_id undefined");
        }
        Object _idValue = entityMap.get(SqlOperationConstant._ID);
        BasicDBObject filter = new BasicDBObject(SqlOperationConstant._ID, ObjectId.isValid(String.valueOf(_idValue)) ? new ObjectId(String.valueOf(entityMap.get(SqlOperationConstant._ID))) : _idValue);
        entityMap.remove(SqlOperationConstant._ID);
        return filter;
    }

}
