package com.anwen.mongo.datasource;

import com.anwen.mongo.annotation.datasource.MongoDs;
import com.anwen.mongo.cache.global.DataSourceNameCache;
import com.anwen.mongo.domain.MongoPlusException;
import com.anwen.mongo.handlers.DataSourceHandler;
import com.anwen.mongo.toolkit.ClassTypeUtil;
import com.anwen.mongo.toolkit.StringUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.context.expression.MethodBasedEvaluationContext;
import org.springframework.core.DefaultParameterNameDiscoverer;
import org.springframework.core.ParameterNameDiscoverer;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.annotation.Order;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;

import java.lang.reflect.Method;

/**
 * 多数据源切面
 *
 * @author JiaChaoYang
 **/
@Aspect
@Order(0)
public class MongoDataSourceAspect {

    private static final ParameterNameDiscoverer PARAMETER_NAME_DISCOVERER = new DefaultParameterNameDiscoverer();

    private static final ExpressionParser EXPRESSION_PARSER = new SpelExpressionParser();

    @Around("@within(com.anwen.mongo.annotation.datasource.MongoDs) || @annotation(com.anwen.mongo.annotation.datasource.MongoDs)")
    public Object manageDataSource(ProceedingJoinPoint joinPoint) throws Throwable {
        MethodSignature methodSignature = (MethodSignature) joinPoint.getSignature();
        Method method = methodSignature.getMethod();

        // 获取方法或类上的注解
        MongoDs mongoDs = getMongoDsAnnotation(method);
        String mongoDsValue = mongoDs.value();
        if (StringUtils.isNotBlank(mongoDsValue) && mongoDsValue.contains("#")) {
            StandardEvaluationContext context = new MethodBasedEvaluationContext(joinPoint, method, joinPoint.getArgs(), PARAMETER_NAME_DISCOVERER);
            mongoDsValue = EXPRESSION_PARSER.parseExpression(mongoDsValue).getValue(context, String.class);
        }

        if (mongoDs.dsHandler() != Void.class){
            DataSourceHandler dataSourceHandler = (DataSourceHandler) ClassTypeUtil.getInstanceByClass(mongoDs.dsHandler());
            mongoDsValue = dataSourceHandler.getDataSource(mongoDsValue);
        }

        if (StringUtils.isBlank(mongoDsValue)) {
            throw new MongoPlusException("Data source not found");
        }

        DataSourceNameCache.setDataSource(mongoDsValue);

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
