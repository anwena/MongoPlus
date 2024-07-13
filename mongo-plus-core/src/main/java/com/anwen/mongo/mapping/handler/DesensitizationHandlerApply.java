package com.anwen.mongo.mapping.handler;

import com.anwen.mongo.annotation.comm.Desensitization;
import com.anwen.mongo.domain.MongoPlusConvertException;
import com.anwen.mongo.handlers.DesensitizationHandler;
import com.anwen.mongo.handlers.ReadHandler;
import com.anwen.mongo.mapping.FieldInformation;
import com.anwen.mongo.toolkit.ClassTypeUtil;
import com.anwen.mongo.toolkit.DesensitizedUtil;

/**
 * 脱敏处理器
 *
 * @author anwen
 * @date 2024/6/30 下午6:01
 */
public class DesensitizationHandlerApply implements ReadHandler {

    @Override
    public Integer order() {
        return 1;
    }

    @Override
    public Object read(FieldInformation fieldInformation, Object source) {
        Desensitization desensitization = (Desensitization) fieldInformation.getAnnotation(Desensitization.class);
        if (fieldInformation.isAnnotation(Desensitization.class)){
            Class<?> desensitizationClass = desensitization.desensitizationHandler();
            if (desensitizationClass != Void.class && DesensitizationHandler.class.isAssignableFrom(desensitizationClass)){
                DesensitizationHandler desensitizationHandler = (DesensitizationHandler) ClassTypeUtil.getInstanceByClass(desensitizationClass);
                source = desensitizationHandler.desensitized(fieldInformation.getField(),
                        source, desensitization.startInclude(), desensitization.endExclude(), desensitization.type());
            } else {
                String desensitizationValue;
                try {
                    desensitizationValue = String.valueOf(source);
                } catch (Exception e) {
                    throw new MongoPlusConvertException("Fields that require desensitization cannot be converted to strings");
                }
                source = DesensitizedUtil.desensitized(desensitizationValue, desensitization.startInclude(), desensitization.endExclude(), desensitization.type());
            }
        }
        return source;
    }
}
