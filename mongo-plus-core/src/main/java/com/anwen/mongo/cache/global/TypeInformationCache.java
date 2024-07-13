package com.anwen.mongo.cache.global;

import com.anwen.mongo.mapping.TypeInformation;

import java.util.HashMap;
import java.util.Map;

/**
 * @author anwen
 * @date 2024/7/13 下午7:18
 */
public class TypeInformationCache {

    public static final Map<Class<?>, TypeInformation> typeInformationMap = new HashMap<>();

}
