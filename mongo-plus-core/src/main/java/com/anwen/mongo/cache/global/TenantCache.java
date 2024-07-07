package com.anwen.mongo.cache.global;

/**
 * 多租户
 * @author anwen
 * @date 2024/6/27 下午12:13
 */
public class TenantCache {

    private static final ThreadLocal<Boolean> ignoreTenantThreadLocal = new InheritableThreadLocal<>();

    public static void setIgnoreTenant(Boolean b) {
        ignoreTenantThreadLocal.set(b);
    }

    public static Boolean getIgnoreTenant() {
        return ignoreTenantThreadLocal.get();
    }

    public static void clear() {
        ignoreTenantThreadLocal.remove();
    }
}
