package com.anwen.mongo.cache;

import com.alibaba.fastjson.serializer.PropertyFilter;
import com.alibaba.fastjson.serializer.SerializeConfig;
import org.bson.codecs.Codec;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author JiaChaoYang
 **/
public class CodecCache {

    public static Map<Class<?>, Codec<?>> codecMap = new ConcurrentHashMap<>();

}
