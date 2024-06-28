package com.anwen.mongo.annotation.comm;

import com.anwen.mongo.enums.DesensitizationTypeEnum;

import java.lang.annotation.*;

/**
 * 数据脱敏注解
 *
 * @author anwen
 * @date 2024/6/28 下午2:59
 */
@Target(ElementType.FIELD)
//运行时注解
@Retention(RetentionPolicy.RUNTIME)
//表明这个注解应该被 javadoc工具记录
//生成文档
@Documented
public @interface Desensitization {

    /**
     * 脱敏类型
     * @return {@link DesensitizationTypeEnum}
     * @author anwen
     * @date 2024/6/28 下午3:51
     */
    DesensitizationTypeEnum type() default DesensitizationTypeEnum.CUSTOM;

    /**
     * 脱敏开始位置(包含)，{@link #type()}为{@link DesensitizationTypeEnum#CUSTOM}并且
     * {@link #desensitizationHandler()}为{@link Void}时，会使用这里指定的位置处理
     * @return {@link int}
     * @author anwen
     * @date 2024/6/28 下午3:52
     */
    int startInclude() default 0;

    /**
     * 脱敏结束位置(不包含)，{@link #type()}为{@link DesensitizationTypeEnum#CUSTOM}并且
     * {@link #desensitizationHandler()}为{@link Void}时，会使用这里指定的位置处理
     * @return {@link int}
     * @author anwen
     * @date 2024/6/28 下午3:53
     */
    int endExclude() default 0;

    /**
     * 脱敏处理器，{@link #type()}为{@link DesensitizationTypeEnum#CUSTOM}时，会走这里配置的处理器
     * <p>脱敏处理器需实现{@link com.anwen.mongo.handlers.DesensitizationHandler}接口</p>
     * @return {@link Class<?>}
     * @author anwen
     * @date 2024/6/28 下午5:08
     */
    Class<?> desensitizationHandler() default Void.class;

}
