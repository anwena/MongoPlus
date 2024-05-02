package com.anwen.mongo.domain;

/**
 * MongoPlus转换异常
 * @author JiaChaoYang
 * @date 2024/5/2 上午1:32
 */
public class MongoPlusConvertException extends MongoPlusException {
    public MongoPlusConvertException(String message) {
        super(message);
    }

    public MongoPlusConvertException(String message, Throwable cause) {
        super(message, cause);
    }
}
