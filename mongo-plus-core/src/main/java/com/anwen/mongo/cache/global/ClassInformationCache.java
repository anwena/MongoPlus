package com.anwen.mongo.cache.global;

import com.anwen.mongo.mapping.ClassInformation;

import java.util.HashMap;
import java.util.Map;

/**
 * @author JiaChaoYang
 **/
public class ClassInformationCache {

    public static Map<Class<?>, ClassInformation> classInformationMap = new HashMap<>();

}
