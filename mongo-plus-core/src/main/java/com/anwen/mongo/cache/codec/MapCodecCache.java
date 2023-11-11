package com.anwen.mongo.cache.codec;

import org.bson.*;
import org.bson.types.*;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 存储MapCodecProvider中已有的解码器，以及后续添加的解码器，作为缓存
 *
 * @author JiaChaoYang
 **/
public class MapCodecCache {

    public static List<Class<?>> codecClassCache = new ArrayList<Class<?>>(){{
        add(Decimal128.class);
        add(BsonRegularExpression.class);
        add(Double.class);
        add(String.class);
        add(MinKey.class);
        add(Document.class);
        add(Date.class);
        add(Binary.class);
        add(Symbol.class);
//        add(List.class);
        add(Long.class);
        add(MaxKey.class);
        add(Code.class);
        add(Boolean.class);
        add(BsonDbPointer.class);
        add(Integer.class);
        add(BsonTimestamp.class);
        add(ObjectId.class);
        add(CodeWithScope.class);
        add(BsonUndefined.class);
    }};

}
