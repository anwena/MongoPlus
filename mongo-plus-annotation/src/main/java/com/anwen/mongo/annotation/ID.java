package com.anwen.mongo.annotation;

import com.anwen.mongo.enums.IdTypeEnum;

import java.lang.annotation.*;

/**
 * @author JiaChaoYang
 * 标识这是MongoDB的ID属性
 * @since 2023-02-13 11:54
 **/
@Target(ElementType.FIELD)
//运行时注解
@Retention(RetentionPolicy.RUNTIME)
//表明这个注解应该被 javadoc工具记录
//生成文档
@Documented
public @interface ID {

    /**
     * 自动生成id配置
     * @author JiaChaoYang
     * @date 2023/10/25 15:44
    */
    IdTypeEnum type() default IdTypeEnum.OBJECT_ID;

    /**
     * 是否将该字段存入库中
     * <p style="color: red">注意，查询时还是会根据_id进行查询</p>
     * @author JiaChaoYang
     * @date 2023/10/25 15:45
    */
    boolean saveField() default false;

}
