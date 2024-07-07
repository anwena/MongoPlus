package com.anwen.mongo.handlers;

import com.anwen.mongo.enums.DesensitizationTypeEnum;

import java.lang.reflect.Field;

/**
 * 字段脱敏处理器
 *
 * @author anwen
 * @date 2024/6/28 下午5:04
 */
public interface DesensitizationHandler {

    /**
     * 字段脱敏
     * @param field 字段
     * @param obj 字段值
     * @param startInclude 脱敏开始位置
     * @param endExclude 脱敏结束为止
     * @param desensitizedType 脱敏类型
     * @return {@link java.lang.String}
     * @author anwen
     * @date 2024/6/28 下午5:05
     */
    String desensitized(Field field,Object obj, int startInclude, int endExclude, DesensitizationTypeEnum desensitizedType);

}
