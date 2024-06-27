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
     * 驼峰转下划线
     * @author anwen
     * @date 2024/6/27 下午11:56
     */
    public static Boolean camelToUnderline = false;

    /**
     * 是否忽略空值
     * @author JiaChaoYang
     * @date 2023/10/25 15:42
    */
    public static Boolean ignoringNull = true;

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

    /**
     * 存放自增id的集合
     * @author JiaChaoYang
     * @date 2024/5/1 下午10:40
     */
    public static String autoIdCollectionName = "counters";

    /**
     * 小黑子模式
     * @author JiaChaoYang
     * @date 2024/5/2 上午2:24
     */
    public static Boolean ikun = false;

}
