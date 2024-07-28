package com.anwen.mongo.mapping.handler;

import com.anwen.mongo.annotation.comm.FieldEncrypt;
import com.anwen.mongo.handlers.ReadHandler;
import com.anwen.mongo.mapping.FieldInformation;
import com.anwen.mongo.toolkit.EncryptorUtil;

/**
 * 加密处理器
 *
 * @author anwen
 * @date 2024/6/30 下午5:59
 */
public class FieldEncryptApply implements ReadHandler {

    @Override
    public Integer order() {
        return 0;
    }

    @Override
    public Object read(FieldInformation fieldInformation, Object source) {
        FieldEncrypt fieldEncrypt = fieldInformation.getAnnotation(FieldEncrypt.class);
        if (fieldEncrypt != null && fieldEncrypt.findDecrypt()){
            source = EncryptorUtil.decrypt(fieldInformation.getAnnotation(FieldEncrypt.class),source);
        }
        return source;
    }
}
