package com.anwen.mongo.datasource;

import com.anwen.mongo.annotation.datasource.MongoDs;
import com.anwen.mongo.cache.global.DataSourceNameCache;
import com.anwen.mongo.domain.MongoPlusException;
import com.anwen.mongo.toolkit.StringUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.annotation.Order;

import java.lang.reflect.Method;

/**
 * 多数据源切面
 *
 * @author JiaChaoYang
 **/
@Aspect
@Order(0)
public class MongoDataSourceAspect {

    @Around("@within(com.anwen.mongo.annotation.datasource.MongoDs) || @annotation(com.anwen.mongo.annotation.datasource.MongoDs)")
    public Object manageDataSource(ProceedingJoinPoint joinPoint) throws Throwable {
        MethodSignature methodSignature = (MethodSignature) joinPoint.getSignature();
        Method method = methodSignature.getMethod();

        // 获取方法或类上的注解
        MongoDs mongoDs = getMongoDsAnnotation(method);

        if (mongoDs == null || StringUtils.isBlank(mongoDs.value())) {
            throw new MongoPlusException("Data source not found");
        }

        DataSourceNameCache.setDataSource(mongoDs.value());

        try {
            return joinPoint.proceed();
        } finally {
            DataSourceNameCache.clear();
        }
    }

    private MongoDs getMongoDsAnnotation(Method method) {
        MongoDs mongoDs = AnnotationUtils.findAnnotation(method, MongoDs.class);

        if (mongoDs == null || StringUtils.isBlank(mongoDs.value())) {
            mongoDs = AnnotationUtils.findAnnotation(method.getDeclaringClass(), MongoDs.class);
        }

        return mongoDs;
    }

}
