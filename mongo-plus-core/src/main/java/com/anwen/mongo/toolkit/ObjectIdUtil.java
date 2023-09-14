package com.anwen.mongo.toolkit;

import org.bson.types.ObjectId;

import java.util.Collection;
import java.util.Collections;
import java.util.stream.Collectors;

public class ObjectIdUtil {

    /**
     * 转换单个ObjectId
     */
    public static <T> Object convertObjectId(T id) {
        if (id == null) {
            return null;
        }
        String strId = String.valueOf(id);
        if (ObjectId.isValid(strId)) {
            return new ObjectId(strId);
        }
        return id;
    }

    /**
     * 批量转换ObjectId
     */
    public static Collection<Object> convertObjectId(Collection<?> ids) {
        if (ids == null) {
            return Collections.emptyList();
        }
        return ids.stream().map(ObjectIdUtil::convertObjectId).collect(Collectors.toList());
    }
}
