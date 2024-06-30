package com.anwen.mongo.annotation.comm;

import com.anwen.mongo.enums.AlgorithmEnum;

import java.lang.annotation.*;

/**
 * 字段加密
 * @author anwen
 * @date 2024/6/30 下午1:29
 */
@Target(ElementType.FIELD)
//运行时注解
@Retention(RetentionPolicy.RUNTIME)
//表明这个注解应该被 javadoc工具记录
//生成文档
@Documented
public @interface FieldEncrypt {

    /**
     * 加密类型
     * @return {@link com.anwen.mongo.enums.AlgorithmEnum}
     * @author anwen
     * @date 2024/6/30 下午1:45
     */
    AlgorithmEnum algorithm() default AlgorithmEnum.BASE64;

    /**
     * 查询是否解密
     * 如果设置为false，那么在查询时不会进行解密
     * @author anwen
     * @date 2024/6/30 下午5:40
     */
    boolean findDecrypt() default true;

    /**
     * 秘钥，优先于全局参数
     * @author anwen
     * @date 2024/6/30 下午1:48
     */
    String key() default "";

    /**
     * 私钥，优先于全局参数
     * @author anwen
     * @date 2024/6/30 下午1:48
     */
    String privateKey() default "";

    /**
     * 公钥，优先于全局参数
     * @author anwen
     * @date 2024/6/30 下午1:48
     */
    String publicKey() default "";

    /**
     * 加密处理器，需实现{@link com.anwen.mongo.encryptor.Encryptor}接口
     * @author anwen
     * @date 2024/6/30 下午1:46
     */
    Class<?> encryptor() default Void.class;

}
