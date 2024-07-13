package com.anwen.mongo.annotation;

import com.anwen.mongo.handlers.collection.AnnotationHandler;
import com.anwen.mongo.toolkit.StringUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.context.expression.BeanFactoryResolver;
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
public class SpelAnnotationHandler implements AnnotationHandler {

    public static final ParameterNameDiscoverer PARAMETER_NAME_DISCOVERER = new DefaultParameterNameDiscoverer();

    public static final ExpressionParser EXPRESSION_PARSER = new SpelExpressionParser();

    public final ApplicationContext applicationContext;

    public SpelAnnotationHandler(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T, R> R getProperty(T obj, Function<? super T, ? extends R> func) {
        R apply = func.apply(obj);
        if (apply instanceof String){
            String value = (String) apply;
            if (StringUtils.isNotBlank(value) && value.contains("#")) {
                StandardEvaluationContext context = new StandardEvaluationContext();
                context.setBeanResolver(new BeanFactoryResolver(applicationContext));
                apply = (R) EXPRESSION_PARSER.parseExpression(value).getValue(context, String.class);
            }
        }
        return apply;
    }
}
