package com.anwen.mongo.cache.global;

/**
 * 配置文件缓存
 *
 * @author JiaChaoYang
 **/
public class PropertyCache {

    /**
     * 下划线转驼峰
     * @author JiaChaoYang
     * @date 2023/10/25 15:42
    */
    public static Boolean mapUnderscoreToCamelCase = false;

    /**
     * 是否开启spring事务
     * @author JiaChaoYang
     * @date 2023/10/25 15:43
    */
    public static Boolean transaction = false;

    /**
     * 格式化执行语句，默认false
     * @author JiaChaoYang
     * @date 2023/11/22 11:03
    */
    public static Boolean format = false;

}
