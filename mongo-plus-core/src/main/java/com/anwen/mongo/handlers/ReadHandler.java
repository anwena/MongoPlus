package com.anwen.mongo.handlers;

import com.anwen.mongo.mapping.FieldInformation;

/**
 * 映射处理器
 *
 * @author anwen
 * @date 2024/6/30 下午5:54
 */
public interface ReadHandler {

    /**
     * 该处理器的顺序，从小到大
     * @author anwen
     * @date 2024/6/30 下午5:57
     */
    default Integer order(){
        return Integer.MAX_VALUE;
    }

    /**
     * 映射的处理方法，在映射处理后，写入属性值前
     * @param fieldInformation Field的一些信息
     * @param source 要写入field的值
     * @return {@link java.lang.Object}
     * @author anwen
     * @date 2024/6/30 下午5:56
     */
    Object read(FieldInformation fieldInformation,Object source);

}
