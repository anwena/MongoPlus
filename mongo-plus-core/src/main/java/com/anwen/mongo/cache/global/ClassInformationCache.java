package com.anwen.mongo.cache.global;

import com.anwen.mongo.mapping.TypeInformation;

import java.util.HashMap;
import java.util.Map;

/**
 * @author JiaChaoYang
 **/
public class ClassInformationCache {

    public static Map<Class<?>, TypeInformation> classInformationMap = new HashMap<>();

}
