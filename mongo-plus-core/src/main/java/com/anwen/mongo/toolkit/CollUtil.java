package com.anwen.mongo.toolkit;

import java.util.Collection;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description 集合工具类
 * @date 2023-09-22 11:48
 **/
public class CollUtil {

    public static boolean isNotEmpty(Collection<?> collection){
        return collection != null && !collection.isEmpty();
    }

}
