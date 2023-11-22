package com.anwen.mongo.cache.global;

/**
 * 拦截器排序媒介
 * @author JiaChaoYang
 **/
public class OrderCache {
    
    /**
     * 日志拦截器order
     * @author JiaChaoYang
     * @date 2023/11/22 19:03
    */ 
    public static int LOG_ORDER = 0;
    
    /**
     * 防止全集合更新删除拦截器的order
     * @author JiaChaoYang
     * @date 2023/11/22 19:03
    */ 
    public static int BLOCK_ATTACK_INNER_ORDER = 1;
    
}
