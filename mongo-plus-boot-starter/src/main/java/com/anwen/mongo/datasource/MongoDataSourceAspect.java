package com.anwen.mongo.datasource;

import com.anwen.mongo.annotation.datasource.MongoDs;
import com.anwen.mongo.cache.global.DataSourceNameCache;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.MongoException;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.annotation.Order;

/**
 * 多数据源切面
 *
 * @author JiaChaoYang
 **/
@Aspect
@Order(0)
public class MongoDataSourceAspect {

    @Around("@annotation(com.anwen.mongo.annotation.datasource.MongoDs)")
    public Object manageDataSource(ProceedingJoinPoint joinPoint) throws Throwable {
        // 获取方法上的注解
        MethodSignature methodSignature = (MethodSignature) joinPoint.getSignature();
        MongoDs mongoDs = AnnotationUtils.findAnnotation(methodSignature.getMethod(), MongoDs.class);
        if (mongoDs == null || StringUtils.isBlank(mongoDs.value())){
            throw new MongoException("Data source not found");
        }
        DataSourceNameCache.setDataSource(mongoDs.value());
        Object proceed;
        try {
            proceed = joinPoint.proceed();
        }finally {
            DataSourceNameCache.clear();
        }
        return proceed;
    }

}
