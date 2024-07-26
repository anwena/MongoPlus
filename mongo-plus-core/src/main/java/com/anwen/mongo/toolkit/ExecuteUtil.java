package com.anwen.mongo.toolkit;

import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.domain.MongoPlusFieldException;
import com.mongodb.BasicDBObject;

import java.util.Map;

/**
 * @author JiaChaoYang
 **/
public class ExecuteUtil {

    public static BasicDBObject getFilter(Map<String, Object> entityMap) {
        if (!entityMap.containsKey(SqlOperationConstant._ID)) {
            throw new MongoPlusFieldException("_id undefined");
        }
        Object _idValue = entityMap.get(SqlOperationConstant._ID);
        BasicDBObject filter = new BasicDBObject(SqlOperationConstant._ID, StringUtils.getObjectIdValue(_idValue));
        entityMap.remove(SqlOperationConstant._ID);
        return filter;
    }

}
