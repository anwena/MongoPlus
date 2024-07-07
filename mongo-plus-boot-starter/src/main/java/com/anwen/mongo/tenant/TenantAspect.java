package com.anwen.mongo.tenant;

import com.anwen.mongo.cache.global.TenantCache;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.core.annotation.Order;

/**
 * 多租户切面
 *
 * @author anwen
 * @date 2024/6/27 下午12:48
 */
@Aspect
@Order(1)
public class TenantAspect {

    @Around("@annotation(com.anwen.mongo.annotation.tenant.IgnoreTenant)")
    public Object ignoreTenant(ProceedingJoinPoint joinPoint) throws Throwable {
        try {
            TenantCache.setIgnoreTenant(true);
            return joinPoint.proceed();
        } finally {
            TenantCache.clear();
        }
    }
}
