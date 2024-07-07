package com.anwen.mongo.datasource;

import com.anwen.mongo.annotation.datasource.MongoDs;
import com.anwen.mongo.cache.global.DataSourceNameCache;
import com.anwen.mongo.domain.MongoPlusException;
import com.anwen.mongo.toolkit.StringUtils;
import org.noear.solon.core.aspect.Interceptor;
import org.noear.solon.core.aspect.Invocation;

import java.util.Optional;

/**
 * @author anwen
 * @date 2024/7/6 上午12:23
 */
public class MongoDataSourceAspect implements Interceptor {
    @Override
    public Object doIntercept(Invocation inv) throws Throwable {
        return Optional.ofNullable(inv.method().getAnnotation(MongoDs.class)).map(mongoDs -> {
            if (StringUtils.isBlank(mongoDs.value())) {
                throw new MongoPlusException("Data source not found");
            }
            DataSourceNameCache.setDataSource(mongoDs.value());
            try {
                return inv.invoke();
            } catch (Throwable e) {
                throw new RuntimeException(e);
            } finally {
                DataSourceNameCache.clear();
            }
        });
    }
}
