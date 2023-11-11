package com.anwen.mongo.cache.codec;

import org.bson.codecs.Codec;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author JiaChaoYang
 **/
public class CodecCache {

    public static Map<Class<?>, Codec<?>> codecMap = new ConcurrentHashMap<>();

}
