package com.anwen.mongo.annotation;

import com.anwen.mongo.handlers.collection.AnnotationHandler;
import com.anwen.mongo.toolkit.StringUtils;
import org.springframework.context.expression.MethodBasedEvaluationContext;
import org.springframework.core.DefaultParameterNameDiscoverer;
import org.springframework.core.ParameterNameDiscoverer;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;

import java.util.function.Function;

/**
 * Spel注解处理器
 *
 * @author anwen
 * @date 2024/7/11 下午5:11
 */
public class SeplAnnotationHandler implements AnnotationHandler {

    public static final ParameterNameDiscoverer PARAMETER_NAME_DISCOVERER = new DefaultParameterNameDiscoverer();

    public static final ExpressionParser EXPRESSION_PARSER = new SpelExpressionParser();

    @Override
    public <T, R> R getProperty(T obj, Function<? super T, ? extends R> func) {
        R apply = func.apply(obj);
        if (apply instanceof String){
            String value = (String) apply;
            if (StringUtils.isNotBlank(value) && value.contains("#")) {
                StandardEvaluationContext context = new MethodBasedEvaluationContext(joinPoint, method, joinPoint.getArgs(), PARAMETER_NAME_DISCOVERER);
                mongoDsValue = EXPRESSION_PARSER.parseExpression(mongoDsValue).getValue(context, String.class);
            }
        }
        return AnnotationHandler.super.getProperty(obj, func);
    }
}
