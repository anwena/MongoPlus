package com.anwen.mongo.tenant;

import com.anwen.mongo.annotation.tenant.IgnoreTenant;
import com.anwen.mongo.cache.global.TenantCache;
import org.noear.solon.core.aspect.Interceptor;
import org.noear.solon.core.aspect.Invocation;

import java.util.Optional;

/**
 * 多租户切面
 *
 * @author anwen
 * @date 2024/6/27 下午12:48
 */
public class TenantAspect implements Interceptor {

    @Override
    public Object doIntercept(Invocation inv) throws Throwable {
        return Optional.ofNullable(inv.method().getAnnotation(IgnoreTenant.class)).map(mongoDs -> {
            try {
                TenantCache.setIgnoreTenant(true);
                return inv.invoke();
            } catch (Throwable e) {
                throw new RuntimeException(e);
            } finally {
                TenantCache.clear();
            }
        });
    }
}
